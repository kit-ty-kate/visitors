open PPrint
open PPrintAux
open Term

let rec term0 t =
  match t with
  | TVar x ->
      string x
  | TLambda _
  | TApp (_, _) ->
      parens (term t)

and term1 t =
  match t with
  | TApp (t1, t2) ->
      app (term1 t1) (term0 t2)
  | _ ->
      term0 t

and term2 t =
  match t with
  | TLambda (x, t) ->
      block
        (backslash ^^ string x ^^ dot)
        (term2 t)
        empty
  | _ ->
      term1 t

and term t =
  term2 t

let term (oc : out_channel) (t : raw_term) : unit =
  output oc (term t)
