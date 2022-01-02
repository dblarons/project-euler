let sum_horizontal arr top i j =
  if i > 16 then top
  else max
    (arr.(j).(i) * arr.(j).(i + 1) * arr.(j).(i + 2) * arr.(j).(i + 3))
    top

let sum_vertical arr top i j =
  if j > 16 then top
  else max
    (arr.(j).(i) * arr.(j + 1).(i) * arr.(j + 2).(i) * arr.(j + 3).(i))
    top

let sum_forwards_diagonal arr top i j =
  if j > 16 || i > 16 then top
  else max
    (arr.(j).(i) * arr.(j + 1).(i + 1) * arr.(j + 2).(i + 2) * arr.(j + 3).(i + 3))
    top

let sum_backwards_diagonal arr top i j =
  if j > 16 || i < 3 then top
  else max
    (arr.(j).(i) * arr.(j + 1).(i - 1) * arr.(j + 2).(i - 2) * arr.(j + 3).(i - 3))
    top

let check_position arr top i j =
  let horizontal = sum_horizontal arr top i j
  in let vertical = sum_vertical arr top i j
  in let forwards_diagonal = sum_forwards_diagonal arr top i j
  in let backwards_diagonal = sum_backwards_diagonal arr top i j
  in max (max (max horizontal vertical) forwards_diagonal) backwards_diagonal

let rec largest_product arr top i j =
  if j > 19 then top
  else let res = check_position arr top i j
    in if i >= 19 then largest_product arr res 0 (j + 1)
       else largest_product arr res (i + 1) j

let main () =
  let a = Array.of_list
    [08; 02; 22; 97; 38; 15; 00; 40; 00; 75; 04; 05; 07; 78; 52; 12; 50; 77; 91; 08]
  in let b = Array.of_list
    [49; 49; 99; 40; 17; 81; 18; 57; 60; 87; 17; 40; 98; 43; 69; 48; 04; 56; 62; 00]
  in let c = Array.of_list
    [81; 49; 31; 73; 55; 79; 14; 29; 93; 71; 40; 67; 53; 88; 30; 03; 49; 13; 36; 65]
  in let d = Array.of_list
    [52; 70; 95; 23; 04; 60; 11; 42; 69; 24; 68; 56; 01; 32; 56; 71; 37; 02; 36; 91]
  in let e = Array.of_list
    [22; 31; 16; 71; 51; 67; 63; 89; 41; 92; 36; 54; 22; 40; 40; 28; 66; 33; 13; 80]
  in let f = Array.of_list
    [24; 47; 32; 60; 99; 03; 45; 02; 44; 75; 33; 53; 78; 36; 84; 20; 35; 17; 12; 50]
  in let g = Array.of_list
    [32; 98; 81; 28; 64; 23; 67; 10; 26; 38; 40; 67; 59; 54; 70; 66; 18; 38; 64; 70]
  in let h = Array.of_list
    [67; 26; 20; 68; 02; 62; 12; 20; 95; 63; 94; 39; 63; 08; 40; 91; 66; 49; 94; 21]
  in let i = Array.of_list
    [24; 55; 58; 05; 66; 73; 99; 26; 97; 17; 78; 78; 96; 83; 14; 88; 34; 89; 63; 72]
  in let j = Array.of_list
    [21; 36; 23; 09; 75; 00; 76; 44; 20; 45; 35; 14; 00; 61; 33; 97; 34; 31; 33; 95]
  in let k = Array.of_list
    [78; 17; 53; 28; 22; 75; 31; 67; 15; 94; 03; 80; 04; 62; 16; 14; 09; 53; 56; 92]
  in let l = Array.of_list
    [16; 39; 05; 42; 96; 35; 31; 47; 55; 58; 88; 24; 00; 17; 54; 24; 36; 29; 85; 57]
  in let m = Array.of_list
    [86; 56; 00; 48; 35; 71; 89; 07; 05; 44; 44; 37; 44; 60; 21; 58; 51; 54; 17; 58]
  in let n = Array.of_list
    [19; 80; 81; 68; 05; 94; 47; 69; 28; 73; 92; 13; 86; 52; 17; 77; 04; 89; 55; 40]
  in let o = Array.of_list
    [04; 52; 08; 83; 97; 35; 99; 16; 07; 97; 57; 32; 16; 26; 26; 79; 33; 27; 98; 66]
  in let p = Array.of_list
    [88; 36; 68; 87; 57; 62; 20; 72; 03; 46; 33; 67; 46; 55; 12; 32; 63; 93; 53; 69]
  in let q = Array.of_list
    [04; 42; 16; 73; 38; 25; 39; 11; 24; 94; 72; 18; 08; 46; 29; 32; 40; 62; 76; 36]
  in let r = Array.of_list
    [20; 69; 36; 41; 72; 30; 23; 88; 34; 62; 99; 69; 82; 67; 59; 85; 74; 04; 36; 16]
  in let s = Array.of_list
    [20; 73; 35; 29; 78; 31; 90; 01; 74; 31; 49; 71; 48; 86; 81; 16; 23; 57; 05; 54]
  in let t = Array.of_list
    [01; 70; 54; 71; 83; 51; 54; 69; 16; 92; 33; 48; 61; 43; 52; 01; 89; 19; 67; 48]

  in let arr = Array.of_list
    [a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p; q; r; s; t];

  in print_int (largest_product arr 0 0 0);

  exit 0;;

main ()
