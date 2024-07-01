let solve () = ((fun v6 ->
  ((fun v7 ->
    ((let rec v3 = (fun v4 ->
      if ((v4 > 30) && (v6 (v7 v4))) then v4 else (v3 (v4+1))) in v3) 2)) (let rec v3 = (fun v4 ->
    if (v4 < 2) then 1 else ((v3 (v4-1))+(v3 (v4-2)))) in v3))) (fun v5 ->
  ((let rec v3 = (fun v4 ->
    if (v4 = v5) then true else if ((v5 mod v4) = 0) then false else (v3 (v4+1))) in v3) 2)))
