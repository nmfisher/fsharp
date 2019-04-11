// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.SourceCodeServices

open FSharp.Compiler
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FSharp.Compiler.PrettyNaming
open System.Collections.Generic
open System.Runtime.CompilerServices
open FSharp.Compiler.AbstractIL.Internal.Library

module UnusedOpens =

    let symbolHash = HashIdentity.FromFunctions (fun (x: FSharpSymbol) -> x.GetEffectivelySameAsHash()) (fun x y -> x.IsEffectivelySameAs(y))

    /// Represents one namespace or module opened by an 'open' statement
    type OpenedModule(entity: FSharpEntity, isNestedAutoOpen: bool) = 

        /// Compute an indexed table of the set of symbols revealed by 'open', on-demand
        let revealedSymbols : Lazy<HashSet<FSharpSymbol>> =
           lazy
            let symbols = 
               [| for ent in entity.NestedEntities do
                      yield ent :> FSharpSymbol
                      
                      if ent.IsFSharpRecord then
                          for rf in ent.FSharpFields do
                              yield rf  :> FSharpSymbol
                      
                      if ent.IsFSharpUnion && not (Symbol.hasAttribute<RequireQualifiedAccessAttribute> ent.Attributes) then
                          for unionCase in ent.UnionCases do
                              yield unionCase :> FSharpSymbol

                      if Symbol.hasAttribute<ExtensionAttribute> ent.Attributes then
                          for fv in ent.MembersFunctionsAndValues do
                              // fv.IsExtensionMember is always false for C# extension methods returning by `MembersFunctionsAndValues`,
                              // so we have to check Extension attribute instead. 
                              // (note: fv.IsExtensionMember has proper value for symbols returning by GetAllUsesOfAllSymbolsInFile though)
                              if Symbol.hasAttribute<ExtensionAttribute> fv.Attributes then
                                  yield fv :> FSharpSymbol
                      
                  for apCase in entity.ActivePatternCases do
                      yield apCase :> FSharpSymbol

                  // The IsNamespace and IsFSharpModule cases are handled by looking at DeclaringEntity below
                  if not entity.IsNamespace && not entity.IsFSharpModule then
                      for fv in entity.MembersFunctionsAndValues do 
                          yield fv :> FSharpSymbol |]

            HashSet<_>(symbols, symbolHash)

        member __.Entity = entity
        member __.IsNestedAutoOpen = isNestedAutoOpen
        member __.RevealedSymbolsContains(symbol) = revealedSymbols.Force().Contains symbol

    type OpenedModuleGroup = 
        { OpenedModules: OpenedModule list }
        
        static member Create (modul: FSharpEntity) =
            let rec getModuleAndItsAutoOpens (isNestedAutoOpen: bool) (modul: FSharpEntity) =
                [ yield OpenedModule (modul, isNestedAutoOpen)
                  for ent in modul.NestedEntities do
                    if ent.IsFSharpModule && Symbol.hasAttribute<AutoOpenAttribute> ent.Attributes then
                      yield! getModuleAndItsAutoOpens true ent ]
            { OpenedModules = getModuleAndItsAutoOpens false modul }

    /// Represents single open statement.
    type OpenStatement =
        { /// All namespaces and modules which this open declaration effectively opens, including the AutoOpen ones
          OpenedGroups: OpenedModuleGroup list

          /// The range of open statement itself
          Range: range

          /// The scope on which this open declaration is applied
          AppliedScope: range }

    /// Gets the open statements, their scopes and their resolutions
    let getOpenStatements (openDeclarations: FSharpOpenDeclaration[]) : OpenStatement[] = 
        openDeclarations
        |> Array.filter (fun x -> not x.IsOwnNamespace)
        |> Array.choose (fun openDecl ->
             match openDecl.LongId, openDecl.Range with
             | firstId :: _, Some range ->
                 if firstId.idText = MangledGlobalName then 
                     None
                 else
                     Some { OpenedGroups = openDecl.Modules |> List.map OpenedModuleGroup.Create
                            Range = range
                            AppliedScope = openDecl.AppliedScope }
             | _ -> None)

    /// Only consider symbol uses which are the first part of a long ident, i.e. with no qualifying identifiers
    let filterSymbolUses (getSourceLineStr: int -> string) (symbolUses: FSharpSymbolUse[]) : FSharpSymbolUse[] =
        symbolUses
        |> Array.filter (fun su ->
             match su.Symbol with
             | :? FSharpMemberOrFunctionOrValue as fv when fv.IsExtensionMember -> 
                // Extension members should be taken into account even though they have a prefix (as they do most of the time)
                true
             | :? FSharpMemberOrFunctionOrValue as fv when not fv.IsModuleValueOrMember -> 
                // Local values can be ignored
                false
             | :? FSharpMemberOrFunctionOrValue when su.IsFromDefinition -> 
                // Value definitions should be ignored
                false
             | :? FSharpGenericParameter -> 
                // Generic parameters can be ignored, they never come into scope via 'open'
                false
             | :? FSharpUnionCase when su.IsFromDefinition -> 
                false
             | _ ->
                // For the rest of symbols we pick only those which are the first part of a long ident, because it's they which are
                // contained in opened namespaces / modules. For example, we pick `IO` from long ident `IO.File.OpenWrite` because
                // it's `open System` which really brings it into scope.
                let partialName = QuickParse.GetPartialLongNameEx (getSourceLineStr su.RangeAlternate.StartLine, su.RangeAlternate.EndColumn - 1)
                List.isEmpty partialName.QualifyingIdents)

    /// Split symbol uses into cases that are easy to handle (via DeclaringEntity) and those that don't have a good DeclaringEntity
    let splitSymbolUses (symbolUses: FSharpSymbolUse[]) : FSharpSymbolUse[] * FSharpSymbolUse[] =
        symbolUses |> Array.partition (fun symbolUse ->
            let symbol = symbolUse.Symbol
            match symbol with
            | :? FSharpMemberOrFunctionOrValue as f ->
                match f.DeclaringEntity with
                | Some ent when ent.IsNamespace || ent.IsFSharpModule -> true
                | _ -> false
            | _ -> false)

    /// Given an 'open' statement, find fresh modules/namespaces referred to by that statement where there is some use of a revealed symbol
    /// in the scope of the 'open' is from that module.
    ///
    /// Performance will be roughly NumberOfOpenStatements x NumberOfSymbolUses
    let isOpenStatementUsed (symbolUses2: FSharpSymbolUse[]) (symbolUsesRangesByDeclaringEntity: Dictionary<FSharpEntity, range list>) 
                            (usedModules: Dictionary<FSharpEntity, range list>) (openStatement: OpenStatement) =

        // Don't re-check modules whose symbols are already known to have been used
        let openedGroupsToExamine =
            openStatement.OpenedGroups |> List.choose (fun openedGroup ->
                let openedEntitiesToExamine =
                    openedGroup.OpenedModules 
                    |> List.filter (fun openedEntity ->
                        not (usedModules.BagExistsValueForKey(openedEntity.Entity, fun scope -> rangeContainsRange scope openStatement.AppliedScope)))
                             
                match openedEntitiesToExamine with
                | [] -> None
                | _ when openedEntitiesToExamine |> List.exists (fun x -> not x.IsNestedAutoOpen) -> Some { OpenedModules = openedEntitiesToExamine }
                | _ -> None)

        // Find the opened groups that are used by some symbol use
        let newlyUsedOpenedGroups = 
            openedGroupsToExamine |> List.filter (fun openedGroup ->
                openedGroup.OpenedModules |> List.exists (fun openedEntity ->
                    symbolUsesRangesByDeclaringEntity.BagExistsValueForKey(openedEntity.Entity, fun symbolUseRange ->
                        rangeContainsRange openStatement.AppliedScope symbolUseRange &&
                        Range.posGt symbolUseRange.Start openStatement.Range.End) || 
                    
                    symbolUses2 |> Array.exists (fun symbolUse ->
                        rangeContainsRange openStatement.AppliedScope symbolUse.RangeAlternate &&
                        Range.posGt symbolUse.RangeAlternate.Start openStatement.Range.End &&
                        openedEntity.RevealedSymbolsContains symbolUse.Symbol)))

        // Return them as interim used entities
        let newlyOpenedModules = newlyUsedOpenedGroups |> List.collect (fun openedGroup -> openedGroup.OpenedModules)
        for openedModule in newlyOpenedModules do
            let scopes =
                match usedModules.TryGetValue openedModule.Entity with
                | true, scopes -> openStatement.AppliedScope :: scopes
                | _ -> [openStatement.AppliedScope]
            usedModules.[openedModule.Entity] <- scopes
        newlyOpenedModules.Length > 0
                                          
    /// Incrementally filter out the open statements one by one. Filter those whose contents are referred to somewhere in the symbol uses.
    /// Async to allow cancellation.
    let rec filterOpenStatementsIncremental symbolUses2 (symbolUsesRangesByDeclaringEntity: Dictionary<FSharpEntity, range list>) (openStatements: OpenStatement list)
                                            (usedModules: Dictionary<FSharpEntity, range list>) acc = 
        async { 
            match openStatements with
            | openStatement :: rest ->
                if isOpenStatementUsed symbolUses2 symbolUsesRangesByDeclaringEntity usedModules openStatement then
                    return! filterOpenStatementsIncremental symbolUses2 symbolUsesRangesByDeclaringEntity rest usedModules acc
                else
                    // The open statement has not been used, include it in the results
                    return! filterOpenStatementsIncremental symbolUses2 symbolUsesRangesByDeclaringEntity rest usedModules (openStatement :: acc)
            | [] -> return List.rev acc
        }

    let entityHash = HashIdentity.FromFunctions (fun (x: FSharpEntity) -> x.GetEffectivelySameAsHash()) (fun x y -> x.IsEffectivelySameAs(y))

    /// Filter out the open statements whose contents are referred to somewhere in the symbol uses.
    /// Async to allow cancellation.
    let filterOpenStatements (symbolUses1: FSharpSymbolUse[], symbolUses2: FSharpSymbolUse[]) openStatements =
        async {
            // the key is a namespace or module, the value is a list of FSharpSymbolUse range of symbols defined in the 
            // namespace or module. So, it's just symbol uses ranges groupped by namespace or module where they are _defined_. 
            let symbolUsesRangesByDeclaringEntity = Dictionary<FSharpEntity, range list>(entityHash)
            for symbolUse in symbolUses1 do
                match symbolUse.Symbol with
                | :? FSharpMemberOrFunctionOrValue as f ->
                    match f.DeclaringEntity with
                    | Some entity when entity.IsNamespace || entity.IsFSharpModule -> 
                        symbolUsesRangesByDeclaringEntity.BagAdd(entity, symbolUse.RangeAlternate)
                    | _ -> ()
                | _ -> ()

            let! results = filterOpenStatementsIncremental symbolUses2 symbolUsesRangesByDeclaringEntity (List.ofArray openStatements) (Dictionary(entityHash)) []
            return results |> List.map (fun os -> os.Range)
        }

    /// Get the open statements whose contents are not referred to anywhere in the symbol uses.
    /// Async to allow cancellation.
    let getUnusedOpens (checkFileResults: FSharpCheckFileResults, getSourceLineStr: int -> string) : Async<range list> =
        async {
            let! symbolUses = checkFileResults.GetAllUsesOfAllSymbolsInFile()
            let symbolUses = filterSymbolUses getSourceLineStr symbolUses
            let symbolUses = splitSymbolUses symbolUses
            let openStatements = getOpenStatements checkFileResults.OpenDeclarations
            return! filterOpenStatements symbolUses openStatements
        }