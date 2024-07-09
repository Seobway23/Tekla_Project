(defun c:FG (/ base-set intersect-set ent1 ent2 pts intersec-pts i j sp ep indi-center-pts)
  ;; 교차점 좌표를 저장할 리스트 초기화
  (setq intersec-pts '())
  (setq table_list '())

  ;; 기준 선 리스트 선택
  (princ "\n기준 선 리스트를 선택하세요:")
  (setq base-set (ssget '((0 . "LWPOLYLINE,LINE") (8 . "GRB_CENTER"))))

  ;; 교차되는 선 리스트 선택
  (princ "\n교차되는 선 리스트를 선택하세요:")
  (setq intersect-set (ssget '((0 . "LWPOLYLINE,LINE"))))

  ;; 두 리스트가 존재하면 다음 작업을 수행
  (if (and base-set intersect-set)
    (progn
      (princ "\n기준 선 리스트와 교차되는 선 리스트를 선택했습니다.\n")
      ;; 기준 선 리스트와 교차되는 선 리스트의 교차점을 찾는다.
      (setq i 0)
      (while (< i (sslength base-set))
	(setq dddd (sslength base-set))
	(princ "\n전체길이i:")
	(princ dddd)
        (setq ent1 (ssname base-set i))
        (setq indi-center-pts '())
	(setq indi-leng-pts '()) ;; indi-leng-pts 초기화
	
        (setq j 0)
        (while (< j (sslength intersect-set))
          (setq ent2 (ssname intersect-set j))
          (setq pts (acet-geom-intersection (vlax-ename->vla-object ent1)
                                            (vlax-ename->vla-object ent2)))
          (if pts
            (progn
              (setq indi-center-pts (append indi-center-pts (list pts)))
            )
            (princ "\n교차점 없음.")
          )
          (setq j (1+ j))
        )

        (princ (strcat "\nindi-points: " (vl-princ-to-string indi-center-pts)))

        ;; sp, ep 정의
        (setq sp (cdr (assoc 10 (entget ent1))))
        (setq ep (cdr (assoc 11 (entget ent1))))

        ;; 추가
        (setq indi-center-pts (append indi-center-pts (list sp)))
        (setq indi-center-pts (cons ep indi-center-pts))

        ;; 점들을 x 좌표를 기준으로 내림차순 정렬
        (setq indi-center-pts (vl-sort indi-center-pts (function (lambda (a b) (> (car a) (car b))))))

        ;; 기본 로직
	(setq intersec-pts (append intersec-pts indi-center-pts))
	(setq i (1+ i))

	;; 최종 리스트에 length table 추가
	(setq k 0) ;; 인덱스는 0부터 시작

	(princ "\n반복횟수:")
	(princ (length indi-center-pts))

	(while (< k (- (length indi-center-pts) 1)) ;; k가 intersec-pts의 길이 - 1만큼 반복
	  (setq pt1 (nth k indi-center-pts))      ;; k번째 요소
	  (setq pt2 (nth (1+ k) indi-center-pts)) ;; k+1번째 요소
	  (setq leng (- (cadr pt1) (cadr pt2))) ;; y좌표의 차이 계산
	  (setq indi-leng-pts (append indi-leng-pts (list leng)))
	  (setq k (1+ k))
	)
	(setq table_list (append table_list indi-leng-pts))
	
	
      )

  
      
      
      ;; 교차점 좌표를 출력한다.
      (if intersec-pts
	(progn
        (princ (strcat "\n최종 교차점: " (vl-princ-to-string intersec-pts)))
	(princ "\nTable:")
	(princ table_list)
        )

      )
    )
    (princ "\n두 개의 선 리스트를 모두 선택해야 합니다.")
  )	
  (princ)
)

(defun acet-geom-intersection (obj1 obj2 / int-pts)
  ;; 두 객체의 교차점을 찾는 함수
  (setq int-pts (vlax-invoke obj1 'IntersectWith obj2 acExtendNone))
  int-pts
)
