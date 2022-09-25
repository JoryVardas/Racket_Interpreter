#lang racket

;Important notes:
;
;Pass parameters are parameters that are passed to functions and all the information needed for them.
;  there are two types of pass parameter: func and const
;  A const pass parameter looks like:
;    (const const_type value)
;      where const_type is the type of the value, ie. list, pair, number, etc.  (only the ones that we use are considered, all others fall under 'other' and are not considered special.
;        Having this lets us do some checks before calling a function that expects one thing but would get another.
;  A func pass parameter looks like:
;    (func builtin hold_parameters num_parameters parameter_list body)
;      Where builtin is either 'builtin or some other value, and is used to indicate if the function that is being passed is for a function that we define in the interpreter (if it is 'builtin).
;      Where hold_parameters is used to indicate if the parameters being passed to the function should be evaluated before the function call (#f if they should be evaluated before, #t if they should not be).
;      Where num_parameters is the number of parameters the function will accept (there are some special cases that were not need for this project, but which still exist in the code).
;      Where parameter_list is a list parameter names, these become bindings with the result of evaluating the parameters being passed.
;      Where body is the body of the function.
;
;Bindings are reperesented the same way as pass parameters with a binding name appended to the front.
;  In other words the cdr of a binding is a pass parameter.

;A function for looping through and printingt a list as a string.
;
;Input:
; msg_list: A list of elements that should be printed as a string.
(define (fatal_error_msg_loop action msg_list)
  (
    if (equal? (length msg_list) 0)
       (display "\n")
       (fatal_error_msg_loop (display (car msg_list)) (cdr msg_list))
  )
)

;A function for exiting racket.
;
;Input:
; par1: used to perform an expression before exiting.
(define (fatal_error_exit par1)
  (exit)
)

;A function to print the msg_list and then exit racket.
;
;Input:
; msg_list: the list that represents a string that should be printed and then quit racket.
(define (fatal_error msg_list)
  (
   fatal_error_exit (fatal_error_msg_loop '() msg_list)
  )
)

;A function to get the type of a pass parameter
;
;Input:
; pass: the pass parameter to get the type of
(define (get_pass_type pass)
    (car pass)
)


;A function to get the body of a pass parameter
;
;Input:
; pass: the pass parameter to get the body of the pass parameter (body if the pass parameter's type is 'function, and the value if it is 'const.
(define (get_pass_body pass)
  (
    ;see which kind we are dealing with
     if (equal? (get_pass_type pass) 'func)
        (get_function_body pass)

        ;if we were not a function type, then we must be a constant
        (get_const_value pass)
   )
)

;A function to check if the number of parameters needed for func matches the number of parameters provided.
;
;Input:
; func: a pass parameter representing a function.
; params: a list of parameters to be passed to the funciton represented by func..
(define (has_enough_parameters func params)
  (
    if(equal? (get_function_type func) 'builtin)
      (
       ;this here was to check some special cases for the builtin functions, but is not used as it was not needed given the requirements of the project.
       ;it doesn't hurt to keep it, but it will never be true given the current state of the project.
        if (or (equal? (get_function_number_params func) '+) (and (equal? (get_function_number_params func) '1+) (>= (length params) 1)))
           ;the function takes any number of parameters
           #t

           ;otherwise
           (equal? (get_function_number_params func) (length params))
      )

      (equal? (get_function_number_params func) (length params))
  )
)

;A function to get the value of hold_param in the pass parameter func.
;
;Input:
; func: the pass parameter to get the hold_param state of.
(define (get_function_hold_param func)
  (car (cdr (cdr func)))
)

;A function to get the type of the function represented by the pass parameter func (if it is builtin or not)
;
;Input:
; func: the function to get the type of.
(define (get_function_type func)
  (car (cdr func))
)

;A function to get the body of the function represented by teh pass parameter func.
;
;Input:
; func: the function to get the body of.
(define (get_function_body func)
  (car (cdr (cdr (cdr (cdr (cdr func))))))
)

;A function to get the value of the constant represeneted by the pass parameter const.
;
;Input:
; const: the constant to get the value of.
(define (get_const_value const)
  (car (cdr (cdr const)))
)

;A function to get the parameter list of the function represented by the pass parameter func
;
;Input:
; func: the function to get the parameter list of.
(define (get_function_params func)
  (car (cdr (cdr (cdr (cdr func)))))
)

;A function to get the number of parameters of the function represented by the pass parameter func
;
;Input:
; func: the function to get the number of parameters  of.
(define (get_function_number_params func)
  (car (cdr (cdr (cdr func))))
)

(define (get_const_type const)
  (car (cdr const))
)



;with the eval functions we don't actually have to check if the number of parameters are right, since that will have been checked before they are called.

;A function to loop through and add a list of numbers represented by pass parameters.
;
;Input:
; params: the list of numbers to add
(define (eval_add_loop params)
  (
    if (equal? params null)
       ;if there are no parameters return 0
       0

       ;otherwise calculate the value
       ;
       ;first make sure what we have is a number
       (
        if (equal? (get_pass_type (car params)) 'const)
           (
            if (equal? (get_const_type (car params)) 'number)
               ; we are a number
               (+ (get_pass_body (car params)) (eval_add_loop (cdr params)))

               ;else we have an error
               (fatal_error (list "+ was expecting a number, but got: " (get_pass_body (car params))))
           )

           ;else we have an error
          (fatal_error (list "+ was expecting a number, but got the function: " (get_pass_body (car params))))
      )
  )
)
;A function to add a list of numbers represented by pass parameters and return a pass parameter
;
;Input:
; params: the list of numbers to add
(define (eval_add params)
  ;the result of an add operation on numbers must be a number.
  (list 'const 'number (eval_add_loop params))
)


;A function to loop through and subtract a list of numbers represented by pass parameters.
;
;Input:
; params: the list of numbers to subtract
(define (eval_sub_loop params)
  (
    if (equal? params null)
       ;if there are no parameters return 0
       0

       ;otherwise calculate the value
       ;
       ;first make sure what we have is a number
       (
        if (equal? (get_pass_type (car params)) 'const)
           (
            if (equal? (get_const_type (car params)) 'number)
               ; we are a number
               (- (get_pass_body (car params)) (eval_sub_loop (cdr params)))

               ;else we have an error
               (fatal_error (list "- was expecting a number, but got: " (get_pass_body (car params))))
           )

           ;else we have an error
          (fatal_error (list "- was expecting a number, but got the function: " (get_pass_body (car params))))
      )
  )
)
;A function to subtract a list of numbers represented by pass parameter and return a pass parameter
;
;Input:
; params: the list of numbers to subtract
(define (eval_sub params)
  (list 'const 'number (eval_sub_loop params))
)

;A function to loop through and multiply a list of numbers represented by pass parameters.
;
;Input:
; params: the list of numbers to multiply
(define (eval_mul_loop params)
  (
    if (equal? params null)
       ;if there are no parameters return 0
       1

       ;otherwise calculate the value
       ;
       ;first make sure what we have is a number
       (
        if (equal? (get_pass_type (car params)) 'const)
           (
            if (equal? (get_const_type (car params)) 'number)
               ; we are a number
               (* (get_pass_body (car params)) (eval_mul_loop (cdr params)))

               ;else we have an error
               (fatal_error (list "* was expecting a number, but got: " (get_pass_body (car params))))
           )

           ;else we have an error
          (fatal_error (list "* was expecting a number, but got the function: " (get_pass_body (car params))))
      )
  )
)
;A function to multiply a list of numbers represented by pass parameter and return a pass parameter
;
;Input:
; params: the list of numbers to multiply
(define (eval_mul params)
  (list 'const 'number (eval_mul_loop params))
)


;A function to loop through and divide a list of numbers represented by pass parameters.
;
;params: the list of numbers to divide
;
;This does not actally work for a list of mort than 2 elements.
;  To fix this it should multiply all but the first element together and then perform the division on the first element and the multiplied value.
(define (eval_div_loop params)
  (
    if (equal? params null)
       ;if there are no parameters return 1
       1

       ;otherwise calculate the value
       ;
       ;first make sure what we have is a number
       (
        if (equal? (get_pass_type (car params)) 'const)
           (
            if (equal? (get_const_type (car params)) 'number)
               ; we are a number
               (/ (get_pass_body (car params)) (eval_div_loop (cdr params)))

               ;else we have an error
               (fatal_error (list "/ was expecting a number, but got: " (get_pass_body (car params))))
           )

           ;else we have an error
          (fatal_error (list "/ was expecting a number, but got the function: " (get_pass_body (car params))))
      )
  )
)
;A function to divide a list of numbers represented by pass parameter and return a pass parameter
;
;Input:
; params: the list of numbers to divide
(define (eval_div params)
  ( if (equal? (length params) 1)
       ;if we only have one param then return 1/param
       (list 'const 'number (eval_div_loop (cons (list 'const 'number 1) params)))
       ;check divide by zero
       (
        if (equal? (get_pass_body (car (cdr params))) 0)
           ;we devided by zero, so error
           (fatal_error (list "attempt to divide by zero"))
           ;otherwise we can return the result
           (list 'const 'number (eval_div_loop params))
       )
  )
)


;A function to test if the first parameter in a list is greater than the second parameter in a list and return a boolean value indicating the restult.
;
;Input:
; A list of two number pass parameters that are to be tested.
(define (eval_greater_calc params)
  (
       ;first make sure what we have are numbers
       
        if (and (equal? (get_pass_type (car params)) 'const) (equal? (get_pass_type (car (cdr params))) 'const))
           (
            if (and (equal? (get_const_type (car params)) 'number) (equal? (get_const_type (car (cdr params))) 'number))
               ; we are a number
               (> (get_pass_body (car params)) (get_pass_body (car (cdr params))))

               ;else we have an error
               (fatal_error (list "> was expecting numbers, but got: " (get_pass_body (car params)) " " (get_pass_body (car (cdr params)))))
           )

           ;else we have an error
          (fatal_error (list "> was expecting numbers, but got: " (get_pass_body (car params)) " " (get_pass_body (car (cdr params)))))
      
  )
)
;A function to test if the first parameter in a list is greater than the second parameter in a list and return a boolean pass parameter indicating the restult.
;
;Input:
; A list of two number pass parameters that are to be tested.
(define (eval_greater params)
  (list 'const 'boolean (eval_greater_calc params))
)

;A function to test if the first parameter in a list is less than the second parameter in a list and return a boolean value indicating the restult.
;
;Input:
; A list of two number pass parameters that are to be tested.
(define (eval_less_calc params)
  (
       ;first make sure what we have are numbers
       
        if (and (equal? (get_pass_type (car params)) 'const) (equal? (get_pass_type (car (cdr params))) 'const))
           (
            if (and (equal? (get_const_type (car params)) 'number) (equal? (get_const_type (car (cdr params))) 'number))
               ; we are a number
               (< (get_pass_body (car params)) (get_pass_body (car (cdr params))))

               ;else we have an error
               (fatal_error (list "< was expecting numbers, but got: " (get_pass_body (car params)) " " (get_pass_body (car (cdr params)))))
           )

           ;else we have an error
          (fatal_error (list "< was expecting numbers, but got: " (get_pass_body (car params)) " " (get_pass_body (car (cdr params)))))
      
  )
)

;A function to test if the first parameter in a list is less than the second parameter in a list and return a boolean pass parameter indicating the restult.
;
;Input:
; A list of two number pass parameters that are to be tested.
(define (eval_less params)
  (list 'const 'boolean (eval_less_calc params))
)

;A function to test if the first parameter in a list is greater than or equal to the second parameter in a list and return a boolean value indicating the restult.
;
;Input:
; A list of two number pass parameters that are to be tested.
(define (eval_greater_equal_calc params)
  (
       ;first make sure what we have are numbers
       
        if (and (equal? (get_pass_type (car params)) 'const) (equal? (get_pass_type (car (cdr params))) 'const))
           (
            if (and (equal? (get_const_type (car params)) 'number) (equal? (get_const_type (car (cdr params))) 'number))
               ; we are a number
               (>= (get_pass_body (car params)) (get_pass_body (car (cdr params))))

               ;else we have an error
               (fatal_error (list ">= was expecting numbers, but got: " (get_pass_body (car params)) " " (get_pass_body (car (cdr params)))))
           )

           ;else we have an error
          (fatal_error (list ">= was expecting numbers, but got: " (get_pass_body (car params)) " " (get_pass_body (car (cdr params)))))
      
  )
)
;A function to test if the first parameter in a list is greater than or equal to the second parameter in a list and return a boolean pass parameter indicating the restult.
;
;Input:
; A list of two number pass parameters that are to be tested.
(define (eval_greater_equal params)
  (list 'const 'boolean (eval_greater_equal_calc params))
)

;A function to test if the first parameter in a list is less than or equal to the second parameter in a list and return a boolean value indicating the restult.
;
;Input:
; A list of two number pass parameters that are to be tested.
(define (eval_less_equal_calc params)
  (
       ;first make sure what we have are numbers
       
        if (and (equal? (get_pass_type (car params)) 'const) (equal? (get_pass_type (car (cdr params))) 'const))
           (
            if (and (equal? (get_const_type (car params)) 'number) (equal? (get_const_type (car (cdr params))) 'number))
               ; we are a number
               (<= (get_pass_body (car params)) (get_pass_body (car (cdr params))))

               ;else we have an error
               (fatal_error (list "<= was expecting numbers, but got: " (get_pass_body (car params)) " " (get_pass_body (car (cdr params)))))
           )

           ;else we have an error
          (fatal_error (list "<= was expecting numbers, but got: " (get_pass_body (car params)) " " (get_pass_body (car (cdr params)))))
      
  )
)

;A function to test if the first parameter in a list is less than or equal to the second parameter in a list and return a boolean pass parameter indicating the restult.
;
;Input:
; A list of two number pass parameters that are to be tested.
(define (eval_less_equal params)
  (list 'const 'boolean (eval_less_equal_calc params))
)


;A function to test if the first parameter in a list is equal to the second parameter in a list and return a boolean value indicating the restult.
;
;Input:
; A list of two number pass parameters that are to be tested.
(define (eval_equal_calc params)
  (
       ;first make sure what we have are numbers
       
        if (and (equal? (get_pass_type (car params)) 'const) (equal? (get_pass_type (car (cdr params))) 'const))
           (
            if (and (equal? (get_const_type (car params)) 'number) (equal? (get_const_type (car (cdr params))) 'number))
               ; we are a number
               (= (get_pass_body (car params)) (get_pass_body (car (cdr params))))

               ;else we have an error
               (fatal_error (list "= was expecting numbers, but got: " (get_pass_body (car params)) " " (get_pass_body (car (cdr params)))))
           )

           ;else we have an error
          (fatal_error (list "= was expecting numbers, but got: " (get_pass_body (car params)) " " (get_pass_body (car (cdr params)))))
      
  )
)

;A function to test if the first parameter in a list is equal to the second parameter in a list and return a boolean pass parameter indicating the restult.
;
;Input:
; A list of two number pass parameters that are to be tested.
(define (eval_equal params)
  (list 'const 'boolean (eval_equal_calc params))
)

(define (eval_lambda_check_param_names params)
  (
    if(equal? (length params) 0)
      #t
      
      (
         and
          (valid_binding_name (car params))
          (eval_lambda_check_param_names (cdr params))
      )
  )
)

;A function that creates a function pass parameter.
;
;Input:
; params: a list containing a list of parameter names, and an expression representing the body of a function.
(define (eval_lambda params)
  (
   ;check the names for the lambda parameters
   if (eval_lambda_check_param_names (car params))
      (
        ;the first thing to note is that the parameters for the lambda have not been evaluated, which is exactly what we want,
        ; this actually doesn't affect anything since there is nothing to evaluate at this stage anyway
        list 'func 'custom #f (length (car params)) (car params) (car (cdr params))
       )

       ;we were given parameter names that are not valid
       (fatal_error (list "One or more of: " (car params) " are not valid names for parameters"))
  )
)

;A function to take two pass parameters and construct a pair such that the value of the first pass parameter is the first element in the pair and the value of the second pass parameter is the second.
;
;Input:
; params: a list of two pass parameters that are to be joined as a list.
(define (eval_cons params)
  (
   ;why reinvent the functionality when we can just use cons.
   list 'const 'pair (cons (get_pass_body (car params)) (get_pass_body (car (cdr params))))
  )
)

;A function to return a pass parameter representing the first element of the list represented by the input pass parameter.
;
;Input:
; params: a pass parameter representing a list.
(define (eval_car params)
  (
    ;check that we are given a list, if not then we have an error
    if (and (equal? (get_pass_type (car params)) 'const) (equal? (get_const_type (car params)) 'pair))
       ;we are dealing with a list
       (list 'const (determine_const_type (car (get_pass_body (car params)))) (car (get_pass_body (car params))))

       ;else error
       (fatal_error (list "car was expecting a pair but was given: " (get_pass_body (car params))))
  )
)
;A function to return a pass parameter representing all but the first element of the list represented by the input pass parameter.
;
;Input:
; params: a pass parameter representing a list.
(define (eval_cdr params)
  (
    ;check that we are given a list, if not then we have an error
    if (and (equal? (get_pass_type (car params)) 'const) (equal? (get_const_type (car params)) 'pair))
       ;we are dealing with a list
       (list 'const (determine_const_type (cdr (get_pass_body (car params)))) (cdr (get_pass_body (car params))))

       ;else error
       (fatal_error (list "cdr was expecting a pair but was given: " (get_pass_body (car params))))
  )
)
;A function to return a pass parameter representing the parameter passed in.
;
;Input:
; params: some value or list to be converted to a const pass parameter (it will not be evaluated if it is a list)
(define (eval_quote params)
  (
    list 'const (determine_const_type (car params)) (car params)
  )
)

;A function to return a pass parameter representing if the input pass parameter represents a pair (note that most lists are considered pairs).
;
;Input:
; params: the pass parameter to test.
(define (eval_pair_question params)
  (
    list 'const 'boolean (and (equal? (get_pass_type (car params)) 'const) (equal? (get_const_type (car params)) 'pair))
  )
)


;A function to evaluate an expression if a condition is met, and to evaluate another expression if the condition is not met.
;
;Input:
; condition: a pass parameter representing #t if the first expression is to be evaluated, #f if the second expression is to be evaluated..
; params: a list of two expressions.  The first will be evaluated if the condition is met, otherwise the second will be evalueated.
; current_bindings: a list of the current bindings (needed to evaluate the expressions).
;
;Note that if condition is a pass parameter not representing a boolean value it is treated as though it represents #t.
; this was decided after experimenting with Racket and finding that this is the behaviour in Racket.
(define (eval_if_decider condition params current_bindings)
  (
    if (and (equal? (get_pass_type condition) 'const) (equal? (get_const_type condition) 'boolean))
       ;we have a boolean value so eval the correct path
       (
         if (get_const_value condition)
            (eval_expression (car params) current_bindings)
            (eval_expression (car (cdr params)) current_bindings)
       )
       
       ;this should probably be an error, but testing with normal racket shows that if the cond is anything other than boolean, treat it as true
       (eval_expression (car params) current_bindings)
  )
)

;A funciton for evaluating an if statement.
;
;Input:
; params: a list of three expressions.
;   the first element is an expression used as the condition
;   the second element is an expression used if the condition is true
;   the third element is an expression used if the condition is false.
; current_bindings: a list of the current_bindings.
(define (eval_if params current_bindings)
  (
    ;we havent evaluatated any of the parameters since we have to decide which one to evaluate.
    eval_if_decider (eval_expression (car params) current_bindings) (cdr params) current_bindings
  )
)

;A function to construct the list of bindings needed for evaluating a let statement.
;
;Input:
; tobe: a list of name expression pairs representing the pairs that are to become bindings for the let statement.
;   The expression will be evaluated at this stage.
; current_bindings: a list of the current bindings.
;
;let bindings are evaluated before the body (regardless of side effects)
(define (eval_let_gen_bindings tobe current_bindings)
  (
    if (equal? tobe null)
       ;if there are no new bindings to add, return the current bindings
       current_bindings

       ;otherwise, evaluate the expression and create a binding with the corresponding name, then do the same for all other pairs in tobe, and finally construct a list from the result.
       ;  the list is constructed as a (pair of biding, (pair of binding...))
       (
         ;check that the name of the binding is valid
         if (valid_binding_name (car (car tobe)))
            (cons (cons (car (car tobe)) (eval_expression (car (cdr (car tobe))) current_bindings)) (eval_let_gen_bindings (cdr tobe) current_bindings))

            (fatal_error (list (car (car tobe)) " is not a valid name for a binding"))
       )
  )
)

;A function to evaluate let. First the bindings are created and then the body is evaluated.
;
;Input:
; params: a list of the parameters for a let statement.
;  The first element is a list of name expression pairs that are to become bindings for when the body is evaluated.
;  The second element is the body of the let statement.
;
;Note that while in Racket any number of bodys may be specified, we are one supposed to handle the single body case.
(define (eval_let params current_bindings)
  (
    if (list? (car params))
     (
       ;there are two params the first being the bindings and the second being the body.
       eval_expression (car (cdr params)) (eval_let_gen_bindings (car params) current_bindings)
     )

     ;we were given something that we shouldn't have been (if it is null then we are fine though, otherwise
     (
       if (equal? (car params) 'null)
          ;we were given null, so process it.
          (eval_expression (car (cdr params)) (eval_let_gen_bindings '() current_bindings))

          ;else we have an error
          (fatal_error (list "Bad syntax for let: given " (car params) " instead of list of (<identifier> <expr>) pairs."))
     )
  )
)

;A function to check that the name is valid as a binding name
;
;Input:
; name: the name to check
(define (valid_binding_name name)
  (
    not
    (
      or
       (string? name)
       (number? name)
       (pair? name)
       (list? name)
       (boolean? name)
    )
  )
)

;A function to construct the list of bindings needed for evaluating a letrec statement.
;
;Input:
; tobe: a list of name expression pairs representing the pairs that are to become bindings for the rest of the letrec statement.
;   The expression will be evaluated at this stage.
;   The first element of this list will also be a binding for all other elemetns of this list, and so on.
; current_bindings: a list of the current bindings.
;
;letrec bindings are evaluated before the body (regardless of side effects)
(define (eval_letrec_gen_bindings tobe current_bindings)
  (
    if (equal? tobe null)
       ;if there are no new bindings to add, return the current bindings
       current_bindings

       ;otherwise, evaluate the expression and create a binding with the corresponding name, then do the same for all other pairs in tobe (passing the newly created binding as a current binding for the rest of tobe,
       ;  and finally construct a list from the result.
       ;  the list is constructed as a (pair of biding, (pair of binding...))
       (
         ;check that the name of the binding is valid
         if (valid_binding_name (car (car tobe)))
            (eval_letrec_gen_bindings (cdr tobe) (cons (cons (car (car tobe)) (eval_expression (car (cdr (car tobe))) current_bindings)) current_bindings))

            (fatal_error (list (car (car tobe)) " is not a valid name for a binding"))
       )
  )
)

;A function to evaluate lerect. First the bindings are created and then the body is evaluated.
;
;Input:
; params: a list of the parameters for a letrec statement.
;  The first element is a list of name expression pairs that are to become bindings.
;  The second element is the body of the letrec statement.
;
;Note that while in Racket any number of bodys may be specified, we are one supposed to handle the single body case.
(define (eval_letrec params current_bindings)
  (
    if (list? (car params))
     (
       ;there are two params the first being the bindings and the second being the body.
       eval_expression (car (cdr params)) (eval_letrec_gen_bindings (car params) current_bindings)
     )

     ;we were given something that we shouldn't have been (if it is null then we are fine though, otherwise
     (
       if (equal? (car params) 'null)
          ;we were given null, so process it.
          (eval_expression (car (cdr params)) (eval_letrec_gen_bindings '() current_bindings))

          ;else we have an error
          (fatal_error (list "Bad syntax for let: given " (car params) " instead of list of (<identifier> <expr>) pairs."))
     )
  )
)

;A function to test if one pass parameter is equal to another pass parameter, and return a pass parameter indicating the result.
;
;Input:
; params: a list of two elements to be tested.
(define (eval_equal_question params)
  (
    ;first check the types
    if (equal? (get_pass_type (car params)) (get_pass_type (car (cdr params))))
       (
         ;are we dealing with a const?
         if (equal? (get_pass_type (car params)) 'const)
            ;check the const type
            (
              if (equal? (get_const_type (car params)) (get_const_type (car (cdr params))))
                (
                  ;the types are (more or less) the same, so return a pass parameter indicating if they are of equal value.
                  list 'const 'boolean (equal? (get_const_value (car params)) (get_const_value (car (cdr params))))
                )
                
                ;the types are different so return false
                (list 'const 'boolean #f)
            )

            ;we are dealing with a function
            (list 'const 'boolean (equal? (get_function_body (car params)) (get_function_body (car (cdr params)))))
       )

       ;one wass a func and the other a const, so they can't be equal.
       (list 'const 'boolean #f)
  )
)

;A funciton to construct the bindings needed to evaluate a function
;
;Input:
; names: a list of names for the bindings
; values: a list of values for the bindings (these will have been evaluated before we get them)
(define (eval_custom_function_gen_bindings names values)
  (
    ;we already know the number of names and values match since that is checked when they were evaluated before we got here
    if (equal? names null)
       '()
       
       (cons (cons (car names) (car values)) (eval_custom_function_gen_bindings (cdr names) (cdr values)))
  )
)

;A funciton to evaluate a funcition (usually used to apply a function created by lambda to some values).
;
;Input:
; func: a pass parameter representing the function
; params: a list of pass parameters representing the values to be passed to the function.
; current_bindings: a list of the current bindigns.
(define (eval_custom_function func params current_bindings)
  (
    eval_expression (get_function_body func) (append (eval_custom_function_gen_bindings (get_function_params func) params) current_bindings)
  )
)

;A function to evaluate the builtin functions (by calling the corresponding eval function).
;
;Input:
; func: a pass parameter representing the funciton
; params: a list of pass parameters to pass to the function (could also be an expression in some cases, like let, letrec, lambda, etc).
; current_bindings: a list of the current bindings.
(define (eval_builtin_function func params current_bindings)
  (
    case (get_function_body func)
     ((+) (eval_add params))
     ((-) (eval_sub params))
     ((*) (eval_mul params))
     ((/) (eval_div params))
     ((>) (eval_greater params))
     ((<) (eval_less params))
     ((>=) (eval_greater_equal params))
     ((<=) (eval_less_equal params))
     ((=) (eval_equal params))
     ((lambda) (eval_lambda params))
     ((cons) (eval_cons params))
     ((car) (eval_car params))
     ((cdr) (eval_cdr params))
     ((quote) (eval_quote params))
     ((pair?) (eval_pair_question params))
     ((if) (eval_if params current_bindings))
     ((let) (eval_let params current_bindings))
     ((letrec) (eval_letrec params current_bindings))
     ((equal?) (eval_equal_question params))
     (else (fatal_error (list "Builtin function " (get_function_body func) " missing handler")))
  )
)

;A function to get the type of a value
;
;Input:
; const: the value to get the type of.
;
;these are by no means all posible types, only the ones that we want to keep track of.
(define (determine_const_type const)
  (
    if (pair? const)
       'pair

       (
         if (list? const)
         'list

         (
           if (number? const)
              'number

              (
                if (string? const)
                   'string

                   (
                     if (boolean? const)
                        'boolean

                        ;(
                           'other
                       ;  )
                   )
              )
         )
       )
  )
)

;A function to test if a binding exists.
;
;Input:
; binding_name: the name of the binding to look for.
; current_bindings: the list of current bindings to search through.
(define (binding_exists binding_name current_bindings)
  (
    ;are there any bindings?
    if (equal? (length current_bindings) 0)
       ;there are no bindings so we return false
       #f
       
       ;there are bindigns, so is teh current binding the right one?
       (
         if (equal? binding_name (car (car current_bindings)))
           ;the current binding is the right one so return true
           #t

           ;otherwise check if it is further down the list
           (binding_exists binding_name (cdr current_bindings))
       )
  )
)

;A function to get the pass parameter of a specified binding
;  a binding is a pass parameter with a name appended to the front so we return the binding without the name.
;
;Input:
; binding_name: the name of the binding to look for.
; current_bindings: the list of current bindings to search through.
(define (get_binding binding_name current_bindings)
  (
    ;check that there any bindings
    if (equal? (length current_bindings) 0)
       ;there are not any bindings, so return an empty binding (this should have been checked for before calling this)
       '()

       ;there are bindings, so is the current binding the right one?
       (
         if (equal? binding_name (car (car current_bindings)))
            ;the current binding is the right one so return it
            (cdr (car  current_bindings))

            ;otherwise check the rest of the list
            (get_binding binding_name (cdr current_bindings))
       )
  )
)

;A function that will evaluate a function (will test if the funciton is builtin or not, and call the appropriate eval function).
;
;Input:
; func: a pass parameter representing the function.
; params: a list of parameters to pass to the function
; current_bindings: a list of the current bindings.
;
;we are given a function and must apply it to the provided parameters
(define (eval_function func params current_bindings)
  (
         ;do we have enough parameters for the operation?
         if (has_enough_parameters func params)
            ;there are enough parameters
            (
              ;is the operation a built in one?
              if (equal? (get_function_type func) 'builtin)
                (
                 ;we are dealing with a built-in operation
                  eval_builtin_function func params current_bindings
                )

                (
                  ;the operation was defined in the program we are interpreting, so we have to evaluate it as such
                  eval_custom_function func params current_bindings
                )
            )

            ;there were not enough parameters, so throw an error
            (
               fatal_error (list "Number of parameter miss-match.  " (get_function_body func) " expects " (get_function_number_params func) " parameters, but was given " (length params))
            )
  )
)

;A function to evalue a list of parameters and then return the results in a list.
;
;Input:
; params: the list of parameters to evaluate
; current_bindings: a list of the current bindings.
(define (eval_parameters params current_bindings)
  (
     if (equal? params null)
        ;if params are null then just return the empty list
        '()

        ;else we can eval the first parameter and then recursively go through the rest of the params
        (
          cons (eval_expression (car params) current_bindings) (eval_parameters (cdr params) current_bindings)
        )
  )
)

;A funciton to perform any operations that need to be done before evaluating a function (such as evaluating parameters if we are supposed to).
;
;Input:
; func: a pass parameter representing a function.
; params: a list of experssions to either be passed to the function directly, or evaluated and then passed to the funciton.
; current_bindings: a list of the current bindings.
(define (eval_function_pre func params current_bindings)
  (
    if(equal? (get_pass_type func) 'func)
      (
        ;I don't care if the function is builtin or not, just if its parameters should be calculated now
        if (get_function_hold_param func)
           ;if hold_param is set then we dont evaluate them, and the handler will have to.
           (eval_function func params current_bindings)
           ;if hold_param is not set then we evaluate the parameters before passing them to the function
           (eval_function func (eval_parameters params current_bindings) current_bindings)
      )

      ;else error
      (fatal_error (list "Expected a operation/proceedure, but were given:  " (get_pass_body func)))
  )
)

;A funciton to evaluate an expression (of any valid and legal form, within the bounds of the project (may terminate in an error)).
;
;Input:
; expression: the expression to evaluate.
; current_bindings: the current_bindings.
(define (eval_expression expression current_bindings)
  (
    if (pair? expression)
    ;if expression is a pair then we need to evaluate it
    (

     ;;I though that we had to check for quote here, but when the program is read in '<expr> is actually converted to (quote <expr>) so we can treat it like any other built in function.
     
           ;firstly do we have a binding for this operation?
           if (binding_exists (car expression) current_bindings)
              ;if there is a binding then use it (it must be a function since it is an operation)
              ; if a binding is used here then it must have been defined previously, so we either know about it or it is an error
              (
                
                ;eval_function will check that what is passed in is a function
                eval_function_pre (get_binding (car expression) current_bindings) (cdr expression) current_bindings
                ;;eval_function (get_binding (car expression) current_bindings) (eval_parameters (cdr expression) current_bindings) current_bindings
              )

              ;so we dont have a binding for this which means that it must be something that can be evaluated
              (
                eval_function_pre (eval_expression (car expression) current_bindings) (cdr expression) current_bindings
                ;;eval_function (eval_expression (car expression) current_bindings) (eval_parameters (cdr expression) current_bindings) current_bindings
              )
    )
    ;else we are dealing with a constant expression, so return it as such
    ;thought it is possible for it to be a binding, so check that first
    (
      if (binding_exists expression current_bindings)
         ;if there was a binding return the binding body.
         
         (get_binding expression current_bindings)

         ;if there was no binding just return the expression (check that it is a valid type first though)
         (
           if (is_valid_non_binding expression)
              (list 'const (determine_const_type expression) expression)
              (fatal_error (list "Cannot reference an identifier before its definition;  " expression ": undefined"))
         )
    )
  )
)

;A function to return if the specified parameter is a valid non binding name (ie. does the parameter constitute a valid expression on its own).
;
;Input:
; param: the parameter to test.
(define (is_valid_non_binding param)
  (
    or
     (string? param)
     (number? param)
     (boolean? param)
     (char? param)
  )
)

;A function to return a list of all the builtin bindings that we need to make this subset of Racket work.
(define (builtin_bindings)
  (
    list
      '(if func builtin #t 3 () if)
      '(+ func builtin #f 2 () +)
      '(- func builtin #f 2 () -)
      '(* func builtin #f 2 () *)
      '(/ func builtin #f 2 () /)
      '(> func builtin #f 2 () >)
      '(< func builtin #f 2 () <)
      '(>= func builtin #f 2 () >=)
      '(<= func builtin #f 2 () <=)
      '(= func builtin #f 2 () =)
      '(lambda func builtin #t 2 () lambda)
      '(cons func builtin #f 2 () cons)
      '(car func builtin #f 1 () car)
      '(cdr func builtin #f 1 () cdr)
      '(pair? func builtin #f 1 () pair?)
      '(quote func builtin #t 1 () quote)
      '(let func builtin #t 2 () let)
      '(letrec func builtin #t 2 () letrec)
      '(equal? func builtin #f 2 () equal?)
      '(true const boolean #t)
      '(false const boolean #f)
      '(null const list ())
  )
)

;A function to convert a result from eval_expression into a form that is more fitting for output.
;
;Input:
; result: a pass parameter (which was returned by eval_expression) to be converted for output.
(define (startEval_return_check result)
  (
    ;check if the result was a function
    if (equal? (get_pass_type result) 'func)
       ;if it was then specifiy that it is
       (list 'proceedure: (get_pass_body result))

       ;if not then just return the result
       (get_pass_body result)
  )
)

;A function to evaluate a Racket program
;
;Input:
; program: the Racket program to evaluate.
(define (startEval program)
  (startEval_return_check (eval_expression program (builtin_bindings)))
)