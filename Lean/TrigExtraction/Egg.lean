import Lean

private opaque EGraph.Pointed : NonemptyType.{0}

def EGraph := EGraph.Pointed.type

-- IMPORTANT: The C interface to egg depends on the order of these fields.
structure EggResult where
  success : Bool
  term    : String
  egraph? : Option EGraph
deriving Inhabited

-- IMPORTANT: The C interface to egg depends on the order of these fields.
structure RewriteRule where
  name : String
  lhs  : String
  rhs  : String

@[extern "run_egg_c"]
opaque runEgg (target : String) (rws : Array RewriteRule) : Lean.MetaM EggResult

@[extern "query_egraph"]
opaque EGraph.query (query : String) : EggResult
