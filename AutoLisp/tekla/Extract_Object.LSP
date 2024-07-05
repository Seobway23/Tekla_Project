(defun c:ShowEntitiesData ( / ss i ent list filename file handle type layer startPoint endPoint startX startY startZ endX endY endZ)
  (prompt "\n여러 객체를 선택하세요: ")
  (setq ss (ssget)) ; 여러 객체 선택
  (setq filename "C:/Users/admin/Desktop/Tekla_Project/AutoLisp/entities.csv") ; 저장할 파일 경로와 이름 지정

	(princ ss.ent)
(setq list (cons (entget ent) list)
  (princ entget ent)
       (princ list)

)