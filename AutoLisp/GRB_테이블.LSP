(defun c:GS (/ base-set intersect-set ent1 ent2 pts intersec-pts i j sp ep indi-center-pts)
  ;; 교차점 좌표를 저장할 리스트 초기화
  (setq intersec-pts '())
  (setq table_list '())

  ;; 기준 선 리스트 선택
  (princ "\n말뚝을 선택하세요:")
  (setq base-set (ssget '((0 . "LWPOLYLINE,LINE") (8 . "GRB_CENTER"))))

  ;; 교차되는 선 리스트 선택
  (princ "\n지반선을 선택하세요:")
  (setq intersect-set (ssget '((0 . "LWPOLYLINE,LINE"))))

  ;; 두 리스트가 존재하면 다음 작업을 수행
  (if (and base-set intersect-set)
    (progn

      ;; table 변수
      (setq rowHeight 600.0) ;; 행 높이 조정
      (setq colWidth 2000.0) ;; 열 너비 조정
      (setq txtHeight 300) ;; 텍스트 높이 설정
      
      (princ "\n기준 선 리스트와 교차되는 선 리스트를 선택했습니다.\n")
      ;; 기준 선 리스트와 교차되는 선 리스트의 교차점을 찾는다.
      (setq i 0)
      (while (< i (sslength base-set))
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

        ;; 점들을 y 좌표를 기준으로 내림차순 정렬
	(setq indi-center-pts (vl-sort indi-center-pts (function (lambda (a b) (> (cadr a) (cadr b))))))

        ;; 기본 로직
	(setq intersec-pts (append intersec-pts indi-center-pts))
	(setq i (1+ i))

	;; 최종 리스트에 length table 추가
	(setq k 0) ;; 인덱스는 0부터 시작



	(while (< k (- (length indi-center-pts) 1)) ;; k가 intersec-pts의 길이 - 1만큼 반복
	  (setq pt1 (nth k indi-center-pts))      ;; k번째 요소
	  (setq pt2 (nth (1+ k) indi-center-pts)) ;; k+1번째 요소
	  (setq leng (- (cadr pt1) (cadr pt2))) ;; y좌표의 차이 계산
	  (setq indi-leng-pts (append indi-leng-pts (list leng)))
	  (setq k (1+ k))
	)
	(setq table_list (cons indi-leng-pts table_list))
	
      )


      (if table_list
	  (progn	   
	    ;; 표 위치 지정
	    (setq tblPos (getpoint "\n표 위치를 클릭하십시오: "))

	    ;; 도면에 표 생성
	    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
	    (setq modelSpace (vla-get-ModelSpace doc))
	    (setq tblObj (vla-AddTable modelSpace (vlax-3d-point tblPos) (+ (length table_list) 2) (+ (length (car table_list)) 2) rowHeight colWidth))

	    ;; 표 텍스트 높이 설정
	    (vla-SetTextHeight tblObj (+ acDataRow acHeaderRow acTitleRow) txtHeight)
	    
	    (vla-SetText tblObj 0 0 "말뚝 천공 재료표")
	    (vla-SetText tblObj 1 0 "번호")
	    
	    (setq col 1)	 
	    (while (<= col (length (car table_list)))
	      (vla-SetText tblObj 1 col (itoa col)) ;; 첫 번째 행에 헤더 추가
	      (setq col (1+ col))
	    )

	     (vla-SetText tblObj 1 col "전체 길이")

			;; 테이블에 값 추가
		  (setq row 2) ;; 첫 번째 데이터 행
		  (foreach sublist table_list
		    
		    (vla-SetText tblObj row 0 (itoa (1- row))) ;; 첫 번째 열에 번호 추가
		    (setq col 1)
		    (foreach value sublist
		  
		      (vla-SetText tblObj row col (rtos (float value) 2 2)) ;; 값 추가
		      (setq col (1+ col))
		    )
	
		    (vla-SetText tblObj row col (rtos (float (sum-list sublist)) 2 2)) ;; 마지막 열에 총 길이 추가
		    (setq row (1+ row))
		  )


	    
	   )
	)

      
    )
    (princ "\n두 개의 선 리스트를 모두 선택해야 합니다.")
  )	
  (princ)
)

(princ "\nLISP 명령을 로드하였습니다. 'GS' 명령을 사용하십시오.")



(defun acet-geom-intersection (obj1 obj2 / int-pts)
  ;; 두 객체의 교차점을 찾는 함수
  (setq int-pts (vlax-invoke obj1 'IntersectWith obj2 acExtendNone))
  int-pts
)

(defun sum-list (lst)
  (apply '+ lst))


(defun compare-first (a b)
  (< (car a) (car b)))