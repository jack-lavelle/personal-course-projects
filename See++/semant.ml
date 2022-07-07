open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
  (Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      | ((_,n1) :: (_,n2) :: _) when n1 = n2 ->
    raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)

  check_binds "global" globals;

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)

  let built_in_decls = 
    let add_bind map (name, retyp, formlist) = StringMap.add name {
      typ = retyp;
      fname = name; 
      formals = formlist;
      (* locals = []; *) body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Void, [(Int, "x")]);
                        ("printb", Void, [(Bool, "x")]);
                        ("printf", Void, [(Float, "x")]);
                        ("printbig", Void, [(Int, "x")]);
                        ("prints", Void, [(String, "x")]);
                        ("draw", Void, [(Canvas, "can"); (String, "filename")]);
                        ("drawcircle", Void, [(CanvasCircle, "cancir"); (String, "filename")]);
                        ("Point", Point, [(Float, "x"); (Float, "y")]);
                        ("Pixel", Pixel, [(Point, "ep1")]);
                        ("Circle", Circle, [(Point, "ep1"); (Float, "x")]);
                        ("Canvas", Canvas, [(Float, "x"); (Float, "y")]);
                        ("CanvasCircle", CanvasCircle, [(Float, "x"); (Float, "y")]);]
  
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    (* check_binds "local" func.locals; *)


  (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise (Failure err)
    in   
  

    (* Build initial symbol table with globals and formals *)
    let globmap = List.fold_left (fun m (ty, name) -> StringMap.add name ty m) 
        StringMap.empty (globals @ func.formals)
    in

    (* Return type of a symbol from supplied symbol table *)
    let type_of_identifier locals s =
      try StringMap.find s locals
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return member symbol map for a particular type *)
    let member_map_of_type ty = match ty with
        Point 
      | Canvas -> List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
            StringMap.empty [(Float, "x"); (Float, "y")]
      | CanvasCircle -> List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
            StringMap.empty [(Float, "x"); (Float, "y")]
      | Pixel -> List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
            StringMap.empty [(Point, "ep1")]
      | Circle -> List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
            StringMap.empty [(Point, "ep1"); (Float, "x")]
      | _ -> raise (Failure ("type " ^ string_of_typ ty ^ " does not have members"))

     in


    (* Return a semantically-checked expression, i.e., with a type *)

    let rec expr locals = function
        Literal  l  -> (Int, SLiteral l)
      | Fliteral l  -> (Float, SFliteral l)
      | BoolLit l   -> (Bool, SBoolLit l)
      | CharLit l   -> (Char, SCharLit l)
      | StringLit l -> (String, SStringLit l)
      | Noexpr      -> (Void, SNoexpr)
      | Id s        -> (type_of_identifier locals s, SId s)
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier locals var
          and (rt, e') = expr locals e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex ^ " for identifier " ^ var
          in (check_assign lt rt err, SAssign(var, (rt, e'))) 
      | Field(obj, mem)  -> 
          let ty = type_of_identifier locals obj in
          let memmap = member_map_of_type ty in
          let smem = match mem with
              Assign(v,e) as ex-> 
                   let ty = type_of_identifier memmap v in
                      (match e with
                         Fliteral _ -> 
                            let lt = StringMap.find v memmap
                            and (rt, e') = expr locals e in
                            let err = "illegal assignment of object field" ^ 
                                string_of_typ lt ^ " = " ^ 
                                string_of_typ rt ^ " in " ^ 
                                string_of_expr ex ^ " for identifier Field." ^ v
                            in (check_assign lt rt err, SAssign(v, (rt, e')))
                        | Id s ->  (ty,SAssign(v,(ty, SId s)) ) 
                        | _ -> raise (Failure ("illegal member access - "
                                  ^ " expression type is not a field"))) 
              | _ -> expr memmap mem 
            in
          (fst smem, SField(obj, smem))

      | Unop(op, e) as ex -> 
          let (t, e') = expr locals e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr locals e1 
          and (t2, e2') = expr locals e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | Mod when same && t1 = Int -> Int
          | Shoehorn when t1 = Canvas && t2 = Pixel -> Canvas
          | ShoehornCircle when t1 = CanvasCircle && t2 = Circle -> CanvasCircle
          | _ -> raise (
        Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr locals e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in

    let check_bool_expr locals e = 
      let (t', e') = expr locals e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)

    let rec check_stmt locals = function
      Block sl -> 
        let rec check_block block_locals ssl= function
        [Return _ as s] -> ssl @ [check_stmt block_locals s]
          | Return _::_ -> raise (Failure "nothing may follow a return")
          | Block sl :: ss -> [check_stmt block_locals (Block sl)] 
                              @ (check_block block_locals ssl ss)
          | s :: ss -> 
            (match s with 
                VDecl(t,name) -> 
                  (match t with
											Void -> raise(Failure ("illegal void local "^name))
										| _ -> let block_locals = StringMap.add name t block_locals
                           in [check_stmt block_locals s] @ check_block block_locals ssl ss)
              | VDeclAssign(t,name,e) ->
								if t == Void then raise(Failure ("illegal void local "^name) )
								else
                let sx = expr block_locals e in
                let typ = 
                  if fst(sx) == t
                          then fst(sx) 
                          else raise(Failure("illegal assignment")) in
                let block_locals = StringMap.add name typ block_locals in
                  [check_stmt block_locals s] @ check_block block_locals ssl ss
              | _ -> [check_stmt block_locals s] @ check_block block_locals ssl ss)
          | []  -> ssl 
        in SBlock(check_block locals [] sl)
    | VDecl(t,s) -> SVDecl(t,s)
    | VDeclAssign(_,s,e) -> 
      let sx = expr locals e in
      let ty = type_of_identifier locals s in 
      SVDeclAssign(ty,s,sx)
    | Expr e -> SExpr (expr locals e)
    | If(p, b1, b2) -> SIf(check_bool_expr locals p, check_stmt locals b1,
                             check_stmt locals b2)
    | For(e1, e2, e3, st) ->
        SFor(expr locals e1, check_bool_expr locals e2, expr locals e3, 
             check_stmt locals st)
    | While(p, s) -> SWhile(check_bool_expr locals p, check_stmt locals s)
    | Return e -> let (t, e') = expr locals e in
        if t = func.typ then SReturn (t, e') 
        else raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
       string_of_typ func.typ ^ " in " ^ string_of_expr e))
    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      sbody = match check_stmt globmap (Block func.body) with
         SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function functions)
