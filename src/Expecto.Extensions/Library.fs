module Expecto.Extensions

open System
open System.Threading.Tasks
open Expecto
open System.Runtime.ExceptionServices
open Expecto.Impl

let private timeoutImpl (timeout : int) (test : TestCode) : TestCode =
    let timeoutAsync testAsync =
        async {
            let delay = Task.Delay(timeout)

            let! task = Task.WhenAny(testAsync |> Async.StartAsTask :> Task, delay) |> Async.AwaitTask
            if task = delay then
                let ts = TimeSpan.FromMilliseconds (float timeout)
                raise <| AssertException(sprintf "Timeout (%A)" ts)
            else
                try
                    do! task |> Async.AwaitTask
                with
                | :? AggregateException as exc ->
                    ExceptionDispatchInfo.Capture(exc.InnerException).Throw()
        }

    match test with
    | Sync test -> async { test() } |> timeoutAsync |> Async
    | SyncWithCancel test ->
      SyncWithCancel (fun ct ->
        Async.StartImmediate(async { test ct } |> timeoutAsync)
      )
    | Async test -> timeoutAsync test |> Async
    | AsyncFsCheck (testConfig, stressConfig, test) ->
      AsyncFsCheck (testConfig, stressConfig, test >> timeoutAsync)

let rec private applyToTestCase (func : TestCode -> TestCode) (test : Test) =
    match test with
    | TestCase (code, state) ->
        TestCase (code |> func, state)
    | TestList (tests, state) ->
        TestList (tests |> List.map (applyToTestCase func), state)
    | TestLabel (label, test, state) ->
        TestLabel (label, test |> applyToTestCase func, state)
    | Test.Sequenced (method, test) ->
        Test.Sequenced (method, test |> applyToTestCase func)

let taskBasedTimeout time = applyToTestCase (timeoutImpl time)

/// Adds a filter that forcibly interrupts tests that are executing longer than specified timeout.
/// Differs from built-in timeout: built-in one marks tests that executed too long as failed, but it doesn't interrupt them,
/// allowing to hanged-up tests to hang-up the whole test-case.
/// This implementation interrupts long-running tests.
let withTaskBasedTimeout timeout (config : ExpectoConfig) = { config with filter = config.filter >> taskBasedTimeout timeout }

(*--------------------------------------------------------------------------------------------------------------------*)

let private stateToFocused = function
    | Pending -> Pending
    | _ -> Focused

let private stateToPending = function
    | _ -> Pending

let rec private changeFocusState convertState (prefix : string) (test : Test) =
    let self = changeFocusState convertState prefix

    let convertLabel (label : string) test state =
        if label.StartsWith prefix then
            let label = label.Substring(prefix.Length).TrimStart()
            let state = convertState state
            TestLabel (label, self test, state)
        else
            TestLabel (label, self test, state)

    match test with
    | TestLabel (label, test, state) -> convertLabel label test state
    | TestList (tests, state) -> TestList (tests |> List.map self, state)
    | TestCase (code, state) -> TestCase (code, state)
    | Test.Sequenced (method, test) -> Test.Sequenced (method, test |> self)

let exclamationMarkToFocused = changeFocusState stateToFocused "!"

let questionMarkToPending = changeFocusState stateToPending "?"

/// Marks as focused those tests whose name starts with "!".
let withExclamationMarkToFocused (config : ExpectoConfig) = { config with filter = config.filter >> exclamationMarkToFocused }

/// Marks as pending those tests whose name starts with "?".
let withQuestionMarkToPending (config : ExpectoConfig) = { config with filter = config.filter >> questionMarkToPending }
