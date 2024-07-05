(defun c:PEA ( / ss)
  (prompt "\n개체를 선택하세요: ")
  (setq ss (ssget))  ; 모든 개체 선택
  (if ss
    (progn
      (command "_.PEDIT" "_M" ss "" "_Y" "_J" "0.05" "" "")  ; PEDIT 명령을 사용하여 개체들을 결합
      (princ "\nSelected entities have been converted to a polyline.")
    )
    (prompt "\nNo entities selected.")
  )
  (princ)  ; 함수 종료
)
