open OUnit
open Test_helper
open Term
open Term.Notations
open Lppterm
  
let a = var ~tag:Eigen "A" 0
let b = var ~tag:Eigen "B" 0
let c = var ~tag:Eigen "C" 0
    
let tests =
  "LPP Term" >:::
    [
      "Print object" >::
        (fun () ->
           let t = obj (app (const "eval") [a; b]) in
             assert_pprint_equal "{eval A B}" t) ;
      
      "Print arrow" >::
        (fun () ->
           let t = arrow (obj a) (obj b)  in
             assert_pprint_equal "{A} -> {B}" t) ;
      
      "Print multiple arrows" >::
        (fun () ->
           let t = arrow (arrow (obj a) (obj b)) (obj c)  in
             assert_pprint_equal "{A} -> {B} -> {C}" t) ;
      
      "Print forall" >::
        (fun () ->
           let evalAB = obj (app (const "eval") [a; b]) in
           let t = forall ["A"] evalAB in
             assert_pprint_equal "forall A, {eval A B}" t) ;
      
      "Print active restricted object" >::
        (fun () ->
           let evalAB = active_obj (app (const "eval") [a; b]) 1 in
           let typeofAB = obj (app (const "typeof") [a; b]) in
           let t = arrow evalAB typeofAB in
             assert_pprint_equal "{eval A B}* -> {typeof A B}" t) ;
      
      "Print inactive restricted object" >::
        (fun () ->
           let evalAB = inactive_obj (app (const "eval") [a; b]) 1 in
           let typeofAB = obj (app (const "typeof") [a; b]) in
           let t = arrow evalAB typeofAB in
             assert_pprint_equal "{eval A B}@ -> {typeof A B}" t) ;

      "Print OR" >::
        (fun () ->
           let a = obj (const "A") in
           let b = obj (const "B") in
           let t = lpp_or a b in
             assert_pprint_equal "{A} or {B}" t) ;
           
    ]
