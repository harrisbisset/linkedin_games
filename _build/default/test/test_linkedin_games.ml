let%test_unit "Test 5x5 queen board" =
  let ( => ) = [%test_eq: Base.bool] in
  Queens.valid_board [ { x = 0; y = 0 }; { x = 0; y = 0 } ] => false;
  Queens.valid_board [ { x = 0; y = 0 }; { x = 1; y = 0 } ] => false;
  Queens.valid_board [ { x = 0; y = 0 }; { x = 2; y = 0 } ] => true;
  Queens.valid_board [ { x = 0; y = 0 }; { x = 1; y = 1 } ] => false;
  Queens.valid_board [ { x = 2; y = 2 }; { x = 3; y = 3 } ] => false;
  Queens.valid_board [ { x = 0; y = 0 }; { x = 3; y = 0 }; { x = 0; y = 3 } ] => true
;;
