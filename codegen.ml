(* SEE++ Code generation: translate takes a semantically checked AST and
    produces LLVM IR
    
    Authors: TODO

Detailed documentation on the OCaml LLVM library:

http://llvm.org/docs/tutorial/index.html
http://llvm.moe/
http://llvm.moe/ocaml/


*)
module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "SEEPP" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context 
  and i1_t       = L.i1_type     context
  and str_t   = L.pointer_type (L.i8_type context)
  and void_t     = L.void_type   context in
  let float_t    = L.double_type context in
  let ptstruct_t = L.struct_type context [| float_t ; float_t |] in 
  let pstruct_t  = L.struct_type context [| ptstruct_t |] in
  let cstruct_t  = L.struct_type context [| ptstruct_t; float_t |] in
  
  let canvasnode_t = L.named_struct_type context "canvasnode" in
  ignore(L.struct_set_body canvasnode_t [| L.pointer_type (canvasnode_t) ; 
      (L.pointer_type pstruct_t) |] false);

  let canvascirclenode_t = L.named_struct_type context "canvascirclenode" in
  ignore(L.struct_set_body canvascirclenode_t [| L.pointer_type (canvascirclenode_t) ; 
      (L.pointer_type cstruct_t) |] false);
    
  let canvas_t = L.struct_type context [| float_t ; float_t ; 
      (L.pointer_type canvasnode_t) |]  
  in
  
  let canvascircle_t = L.struct_type context [| float_t ; float_t ; 
      (L.pointer_type canvascirclenode_t) |]  
  in
  
  (* Return the LLVM type for a See++ type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.String -> str_t
    | A.Char  -> i8_t
    | A.Point -> ptstruct_t
    | A.Pixel -> pstruct_t
    | A.Circle -> cstruct_t
    | A.Canvas -> canvas_t
    | A.CanvasCircle -> canvascircle_t

  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in


  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in
  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in
  let draw_t : L.lltype = 
      L.function_type i32_t [| canvas_t ; str_t |] in
  let draw_func : L.llvalue =
      L.declare_function "draw" draw_t the_module in

  let drawcircle_t : L.lltype = 
      L.function_type i32_t [| canvascircle_t ; str_t |] in
  let drawcircle_func : L.llvalue =
      L.declare_function "drawcircle" drawcircle_t the_module in
      
  let ptcons_t : L.lltype = 
      L.function_type ptstruct_t [|float_t; float_t|] in
  let ptcons_func : L.llvalue = 
      L.declare_function "Point" ptcons_t the_module in
  let ccons_t : L.lltype = 
      L.function_type pstruct_t [|ptstruct_t|] in
  let ccons_func : L.llvalue = 
      L.declare_function "Pixel" ccons_t the_module in
  let canvascons_t : L.lltype =
      L.function_type canvas_t [|float_t; float_t; (* L.pointer_type canvasnode_t *)|] in
  let canvascons_func : L.llvalue  = 
      L.declare_function "Canvas" canvascons_t the_module in 

  let circlecons_t : L.lltype = 
      L.function_type cstruct_t [|ptstruct_t; float_t; (* L.pointer_type cstruc *)|] in
  let circlecons_func : L.llvalue = 
      L.declare_function "Circle" circlecons_t the_module in
  let canvascirclecons_t : L.lltype =
      L.function_type canvascircle_t [|float_t; float_t; (* L.pointer_type canvascirclenode_t *)|] in
  let canvascirclecons_func : L.llvalue  = 
      L.declare_function "CanvasCircle" canvascirclecons_t the_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in 
      let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
         StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      in

    List.fold_left2 add_formal StringMap.empty fdecl.sformals
      (Array.to_list (L.params the_function)) in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n locals = try StringMap.find n locals
                   with Not_found -> StringMap.find n global_vars
    in


    let mem_to_ind ty = match ty  with
      _ -> List.fold_left (fun m (name, ind) -> StringMap.add name ind m)
                  StringMap.empty [("ep1",0); ("ep2",1); ("cp1",2); 
                      ("cp2",3); ("x",0); ("y",1)]
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder locals ((_, e) : sexpr) = match e with
        SLiteral i   -> L.const_int i32_t i
      | SBoolLit b   -> L.const_int i1_t (if b then 1 else 0)
      | SFliteral l  -> L.const_float_of_string float_t l
      | SCharLit l   -> L.const_int i8_t (Char.code l)
      | SStringLit l -> L.build_global_stringptr l "str" builder
      | SNoexpr      -> L.const_int i32_t 0
      | SId s        -> L.build_load (lookup s locals) s builder
      | SAssign (s, e) -> let e' = expr builder locals e in
                          ignore(L.build_store e' (lookup s locals) builder); e'
      | SField(id,sx) ->
          let getI t n = 
            try StringMap.find n (mem_to_ind t) 
            with Not_found -> raise(Failure("member not found"))in
          let getNextVal o t n = L.build_struct_gep o (getI t n) n builder in
          let rec eval out t = function
              SField(sid, sf)-> eval (getNextVal out t sid) 
                  (L.type_of(getNextVal out t sid)) (snd sf)  
            | SId sid -> 
                    let ref = L.build_struct_gep out (getI t sid) sid builder in
                    L.build_load ref sid builder
            | SAssign(s,e) -> 
                  let ref = L.build_struct_gep out (getI t s) s builder in
                  let e' =  expr builder locals e in
                  ignore(L.build_store e' ref builder); e'  
            | _ -> raise(Failure("invalid field usage"))
          in eval (lookup id locals) (L.type_of (lookup id locals)) (snd sx)
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
          let e1' = expr builder locals e1
          and e2' = expr builder locals e2 in
          (match op with 
              A.Add     -> L.build_fadd
            | A.Sub     -> L.build_fsub
            | A.Mult    -> L.build_fmul
            | A.Div     -> L.build_fdiv
            | A.Mod     -> L.build_srem    
            | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
            | A.Neq     -> L.build_fcmp L.Fcmp.One
            | A.Less    -> L.build_fcmp L.Fcmp.Olt
            | A.Leq     -> L.build_fcmp L.Fcmp.Ole
            | A.Greater -> L.build_fcmp L.Fcmp.Ogt
            | A.Geq     -> L.build_fcmp L.Fcmp.Oge
            | _ -> raise (Failure ("illegal usage of operator " ^ 
                            (A.string_of_op op) ^ " on float"))
          ) e1' e2' "tmp" builder
      | SBinop((A.Canvas, _) as can, op, crv) ->
          let (_,can_s) = (match (snd can) with
              SId s -> (expr builder locals can, s)
              |_-> raise(Failure "improper usage of shoehorn - canvas")) 
          and (_,px_s) = (match (snd crv) with
              SId s -> (expr builder locals crv,s)
              |_->raise(Failure "improper usage of shoehorn - pixel")) in
          (match op with
              A.Shoehorn   -> 
                (* construct new node, add it to front of list *)
                let newnode = L.build_alloca canvasnode_t "newnode" builder in
                let next_node_ptr = L.build_struct_gep newnode 0 "new_pixel" builder in
                ignore(L.build_store (L.const_null (L.pointer_type canvasnode_t)) next_node_ptr builder);
                let pixel_ptr = L.build_struct_gep newnode 1 "pixel" builder in
                let pxlv = lookup px_s locals in
                ignore(L.build_store pxlv pixel_ptr builder);
                let canlv = lookup can_s locals in
                let headptr = L.build_struct_gep canlv 2 "head" builder in
                let oldhead = L.build_load headptr "oldptr" builder in
                ignore(L.build_store oldhead next_node_ptr builder);
                ignore(L.build_store newnode headptr builder); canlv
            | _ -> raise (Failure ("improper usage of shoehorn: -> append() " ^ 
                    (string_of_sexpr can) ^ " and " ^ (string_of_sexpr crv)))) 
      | SBinop((A.CanvasCircle,_) as can, op, crl) ->
          let (_,can_s) = (match (snd can) with
              SId s -> (expr builder locals can, s)
              |_-> raise(Failure "improper usage of shoehorn - canvas")) 
          and (_,cl_s) = (match (snd crl) with
              SId s -> (expr builder locals crl,s)
              |_->raise(Failure "improper usage of shoehorn - circle: -> append().circle")) in
          (match op with
              A.ShoehornCircle   -> 
                (* construct new node, add it to front of list *)
                let newnode = L.build_alloca canvascirclenode_t "newnode" builder in
                let next_node_ptr = L.build_struct_gep newnode 0 "new_circle" builder in
                ignore(L.build_store (L.const_null (L.pointer_type canvascirclenode_t)) next_node_ptr builder);
                let circle_ptr = L.build_struct_gep newnode 1 "circle" builder in
                let pxlv = lookup cl_s locals in
                ignore(L.build_store pxlv circle_ptr builder);
                let canlv = lookup can_s locals in
                let headptr = L.build_struct_gep canlv 2 "head" builder in
                let oldhead = L.build_load headptr "oldptr" builder in
                ignore(L.build_store oldhead next_node_ptr builder);
                ignore(L.build_store newnode headptr builder); canlv
            | _ -> raise (Failure ("improper usage of shoehornCircle with " ^ 
                    (string_of_sexpr can) ^ " and " ^ (string_of_sexpr crl)))) 
      | SBinop (e1, op, e2) ->
          let e1' = expr builder locals e1
          and e2' = expr builder locals e2 in
          (match op with
              A.Add     -> L.build_add
            | A.Sub     -> L.build_sub
            | A.Mult    -> L.build_mul
            | A.Div     -> L.build_sdiv
            | A.Mod     -> L.build_srem
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            | A.Equal   -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | A.Less    -> L.build_icmp L.Icmp.Slt
            | A.Leq     -> L.build_icmp L.Icmp.Sle
            | A.Greater -> L.build_icmp L.Icmp.Sgt
            | A.Geq     -> L.build_icmp L.Icmp.Sge
            | _         -> raise (Failure "illegal binary operation")
          ) e1' e2' "tmp" builder 
      
      | SUnop(op, ((t, _) as e)) ->
          let e' = expr builder locals e in
          (match op with
              A.Neg when t = A.Float -> L.build_fneg 
            | A.Neg                  -> L.build_neg
            | A.Not                  -> L.build_not) 
          e' "tmp" builder
      | SCall ("print", [e]) | SCall ("printb", [e]) ->
          L.build_call printf_func [| int_format_str ; (expr builder locals e) |]
              "printf" builder
      | SCall ("prints", [e]) ->
          L.build_call printf_func [| str_format_str ; (expr builder locals e) |]
              "printf" builder
      | SCall ("printbig", [e]) ->
          L.build_call printbig_func [| (expr builder locals e) |] 
              "printbig" builder
      | SCall ("printf", [e]) -> 
          L.build_call printf_func [| float_format_str ; (expr builder locals e) |]
              "printf" builder
      | SCall ("draw", [can;name]) ->
          let can' = expr builder locals can
          and name' = expr builder locals name in
          L.build_call draw_func [| can' ; name' |]
              "draw" builder

      | SCall ("drawcircle", [cancir;name]) ->
          let cancir' = expr builder locals cancir
          and name' = expr builder locals name in
          L.build_call drawcircle_func [| cancir' ; name' |]
              "drawcircle" builder

      | SCall ("Point", [f1;f2]) -> 
          let f1' = expr builder locals f1
          and f2' = expr builder locals f2 in
          L.build_call ptcons_func [| f1' ; f2' |] "Point" builder 
      | SCall ("Pixel", [p1]) -> 
          let p1' = expr builder locals p1 in  
          L.build_call ccons_func [| p1' |] "Pixel" builder

      | SCall ("Circle", [f1;f2]) -> 
          let f1' = expr builder locals f1
          and f2' = expr builder locals f2 in
          L.build_call circlecons_func [| f1' ; f2' |] "Circle" builder 
          
      | SCall ("Canvas", [x ; y]) ->
          let x' = expr builder locals x
          and y' = expr builder locals y in
          L.build_call canvascons_func [| x' ; y' |] "Canvas" builder

      | SCall ("CanvasCircle", [x ; y]) ->
          let x' = expr builder locals x
          and y' = expr builder locals y in
          L.build_call canvascirclecons_func [| x' ; y' |] "CanvasCircle" builder

      | SCall (fname, args) ->
          let (ldev, sfd) = StringMap.find fname function_decls in
          let actuals = List.rev (List.map (fun e -> expr builder locals e) 
              (List.rev args)) in
          let ret = (match sfd.styp with 
              A.Void -> ""
            | _-> fname^"_ret") in 
          L.build_call ldev (Array.of_list actuals) ret builder 
    in
    
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in
  
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt builder locals = function
        SBlock sl -> List.fold_left (fun (b, lv) s -> stmt b lv s) (builder, locals) sl
      | SVDecl(ty, name) ->
          let local_var = L.build_alloca (ltype_of_typ ty) name builder in
          let locals = StringMap.add name local_var locals in
          (builder, locals)
      | SVDeclAssign(ty, name, sx) ->
           
          let local_var = L.build_alloca (ltype_of_typ ty) name builder in
          let locals = StringMap.add name local_var locals in
          ignore (expr builder locals (ty,SAssign(name, sx))); (builder, locals)
      | SExpr e -> ignore(expr builder locals e); (builder, locals)
      | SReturn e -> ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder locals e) builder );
                     (builder, locals)
      | SIf (predicate, then_stmt, else_stmt) ->
           let bool_val = expr builder locals predicate in
           let merge_bb = L.append_block context "merge" the_function in
           let build_br_merge = L.build_br merge_bb in (* partial function *)
           let then_bb = L.append_block context "then" the_function in
           add_terminal (fst (stmt (L.builder_at_end context then_bb) locals then_stmt))
           build_br_merge;
           let else_bb = L.append_block context "else" the_function in
            add_terminal (fst (stmt (L.builder_at_end context else_bb) locals else_stmt))
           build_br_merge;
           ignore(L.build_cond_br bool_val then_bb else_bb builder);
           (L.builder_at_end context merge_bb, locals)
      | SWhile (predicate, body) ->
           let pred_bb = L.append_block context "while" the_function in
           ignore(L.build_br pred_bb builder);
           let body_bb = L.append_block context "while_body" the_function in
           add_terminal (fst (stmt (L.builder_at_end context body_bb) locals body))
           (L.build_br pred_bb);
           let pred_builder = L.builder_at_end context pred_bb in
           let bool_val = expr pred_builder locals predicate in
           let merge_bb = L.append_block context "merge" the_function in
           ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
           (L.builder_at_end context merge_bb, locals)

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder locals
         ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
      in
        
      (* Build the code for each statement in the function *)
      let (builder, _ ) = stmt builder local_vars (SBlock fdecl.sbody) in
        (* Add a return if the last block falls off the end *)
        add_terminal builder (match fdecl.styp with
            A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
