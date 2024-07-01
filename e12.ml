print_int ((let v3 v3 = (fun v4 ->
  (((fun v1 ->
    (fun v2 ->
      (if (v1 < v2) then v1 else v2))) v4) (1+(if (v4 > 2) then (((let v5 v5 = (fun v6 ->
    (fun v7 ->
      (if (v6 = v4) then v7 else ((v5 (v6+1)) (if ((v3 v6) > (v6-1)) then (if ((v4 mod v6) = 0) then ((v7/(v3 v6))*((v3 v6)-1)) else v7) else v7))))) in Memo.memoize v5) 2) v4) else v4)))) in Memo.memoize v3) 1234567)
