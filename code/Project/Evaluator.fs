module Evaluator

open Graph
open AST

open GraphCreationFunctions

let eval (input: InputSchedule): Graph<Event,int*int> =
    createGraph input