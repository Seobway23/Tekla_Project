(defun c:PEA ( / ss)
  (prompt "\n��ü�� �����ϼ���: ")
  (setq ss (ssget))  ; ��� ��ü ����
  (if ss
    (progn
      (command "_.PEDIT" "_M" ss "" "_Y" "_J" "0.05" "" "")  ; PEDIT ������ ����Ͽ� ��ü���� ����
      (princ "\nSelected entities have been converted to a polyline.")
    )
    (prompt "\nNo entities selected.")
  )
  (princ)  ; �Լ� ����
)