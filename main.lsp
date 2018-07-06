; ======================================================================

(defun translateExp (lst)
   
   (output (inf2pref (conversion lst)))

  ) ; ----------- ГЛАВНАЯ ФУНКЦИЯ

(defun conversion(lst)
   (setq lst (mapcar 'input lst))
   (setq lst (output lst))
   (setq lst (strrep lst "{" "("))
   (setq lst (strrep lst "}" ")"))
 ;  (setq lst (strcat "(" lst ")"))   
   (input lst))



; -------------------------------- ПЕРЕВОД В ПРЕФИКСНУЮ ЗАПИСЬ ------- НАЧ
(defun atomlist (x)
  (cond ((null x) nil)
        ((atom (car x)) (atomlist (cdr x)))
        (t nil)))        
 
(defun inf2pref (x) 
    (prog (hd tl cc xx rr) 
      (cond ((atomlist x) (return (inf-aux x nil nil))))
      (setq rr nil)
      (setq xx x) 
 loop (setq hd (car xx))
      (setq tl (cdr xx))
      (cond ((member hd '(sin cos log exp atn asn acs sh ch sqr sign abs))
              (progn 
                   (setq rr (append rr (list (list hd (inf2pref (car tl))))))
                   (setq tl (cdr tl)))) 
            ((atom hd) (setq rr (append rr (list hd)))) 
            (t (setq rr (append rr (list (inf2pref hd))))))
      (cond ((null tl) (return (inf-aux rr nil nil))))
      (setq xx tl) (go loop)))
      
      
(defun inf-aux (ae operators operands)
   (inf-iter (cdr ae) operators (cons (car ae) operands)))
      
(defun inf-iter (ae operators operands)
   (prog nil 
      (cond ((and (null ae) (null operators)) (return (car operands))))
      (cond ((and (not (null ae)) (or (null operators) (> (weight (car ae)) (weight (car operators)))))
            (return (inf-aux (cdr ae) (cons (car ae) operators) operands))))
      (return (inf-iter ae (cdr operators) (cons (list (opcode (car operators)) (cadr operands) (car operands)) (cddr operands))))))
      
(defun weight (x) 
   (cond ((eq x (quote +)) 1) 
         ((eq x (quote -)) 1) 
         ((eq x (quote <=)) 0)
         ((eq x (quote >=)) 0 )
         ((eq x (quote =)) 0)
         ((eq x (quote !=)) 0)  
         ((eq x (quote *)) 2) 
         ((eq x (quote /)) 2) 
         ((eq x (quote ^)) 3) 
         (t 5)))
         
(defun opcode (op) 
 (cond ((eq op (quote +)) (quote +))
       ((eq op (quote -)) (quote -))
       ((eq op (quote <=)) (quote <=))
       ((eq op (quote >=)) (quote >=)) 
       ((eq op (quote =)) (quote =))
       ((eq op (quote !=)) (quote !=))  
       ((eq op (quote *)) (quote *))
       ((eq op (quote /)) (quote /))
       ((eq op (quote ^)) (quote ^)) 
       ))

; -------------------------------- ПЕРЕВОД В ПРЕФИКСНУЮ ЗАПИСЬ ------- КОН
 (defun createListOfVariables(var &optional (nres nil))
    	(if (null (null var))
    		(if (eq (cadr var) '>= )
    		(progn 
        		(setq nres (append nres(list  (list (car var) (caddr var))) )) 
        		(createListOfVariables (cdddr var) nres)
        	    )
        	(progn 
        		(setq nres (append nres(list  (list (car var) 0)) )) 
        		(createListOfVariables (cdr var) nres)
        	))

        nres
        )
       
)
; ======================================================================
; Функция обработки знаков присваивания
(defun fixEquals (str)
	(let ((i 2) (signs "<>!") (leftstr "") (midstr " = ") (rightstr "") (wasReplace nil))
		(loop
			(setq wasReplace nil)
			(when (eq "=" (strmid str i 1))
				(when (and (= 0 (strind signs (strmid str (- i 1)))) (= 0 (strind signs (strmid str (+ i 1)))))
					(setq leftstr (strmid str 1 (- i 1)))
					(setq rightstr (strmid str (+ 1 i)))
					(setq str (strcat leftstr midstr rightstr))
					(setq wasReplace t)
				)
			)
			(when (>= i (- (strlen str) 1))
				(return 0)
			)
			(if wasReplace
				(setq i (+ i 2))
				(incf i)
			)
		)
		str
	)
)

; Обработка исключений вида (=-)
(defun fixminus (str) 
	(let ((isEqual nil) (i 1) (leftstr "") (midstr "0-") (rightstr "") (wasReplace nil))
		(loop
			(setq wasReplace nil)
			; Встретили =
			(when (and (eq "=" (strmid str i 1)) (null isEqual))
				(setq isEqual t)
			)
			; Встретили - после =
			(when (and (eq "-" (strmid str i 1)) isEqual)
				(setq isEqual nil)
				(setq leftstr (strmid str 1 (- i 1)))
				(setq rightstr (strmid str (+ 1 i)))
				(setq str (strcat leftstr midstr rightstr))
			)
			; Встретили другой символ после =
			(when (and (null (eq " " (strmid str i 1))) isEqual (null (eq "=" (strmid str i 1))))
				(setq isEqual nil)
			)
			; Проверка выхода
			(if (>= i (- (strlen str) 1))
				(return str)
			)
			; Инкремент счётчика
			(setq i (+ i 1))
		)
		str
	)
)

; Обработка исключений вида (>= <= !=)
(defun fixeq (str) 
	(let ((current 2) (symbols "<>!") (place 0) (midstr "") (leftstr "") (rightstr "") (wasreplace nil)) 
		(loop
			(setq place (strind symbols (strmid str (- current 1) 1))) ; Место символа в строке
			(setq wasreplace nil)
			( when (and (/= 0 place) (eq "=" (strmid str current 1)))
				(setq leftstr (strmid str 1 (- (- current 1) 1))) ; Левая часть
				(setq rightstr (strmid str (+ 1 current))) ; Правая часть
				(setq midstr (strcat " " (strmid str (- current 1) 1) "=" " ")) ; Середина
				(setq str (strcat leftstr midstr rightstr))
				(setq wasreplace t)
			)
			; Выход из цикла
			(if (>= current (+ 1 (strlen str)))
				(return t)
			)
			; Инкремент счётчика
			(if wasreplace
				(setq current (+ 2 current))
				(setq current (+ 1 current))
				)
		)
		str
	)
)

; Главная функция парсинга
( defun split (str)
	(setq str (strrep str "(-" "(0 - "))
	(setq str (fixminus str))
	(setq str (fixeq str))
	(setq str (fixEquals str))
	(when (eq "-" (strmid str 1 1))
		(setq str (strmid str 2))	
		(setq str (strcat "0 - " str))
	)	
	(setq str (strrep str "(" " { "))
	(setq str (strrep str "+" " + "))
	(setq str (strrep str "-" " - "))
	(setq str (strrep str "*" " * "))
	(setq str (strrep str "/" " / "))
	(setq str (strrep str "^" " ^ "))
	(setq str (strrep str ")" " } "))
	(setq str (strrep str "<>" " <> "))
	(setq str (strcat str " "))
	; Строка готова для парсинга
	( let ((begin 0) (current 1) (res ()) (isWord nil) (digits "1234567890.{}+-/*^<>=!_qwertyuiopasdfghjklzxcvbnm"""))
		(loop
			; Когда встречаем новое слово после пробела
			(when (and (<> 0 (strind digits (strmid str current 1))) (null isWord))
				(setq isWord t)
				(setq begin current)
			)
			; Когда встречаем пробел после слова
			(when (and (= 0 (strind digits (strmid str current 1))) isWord)
				(setq res (append res (list (strmid str begin (- current begin)))))
				(setq isWord nil)
			)
			; Если это конец
			(if (= current (strlen str))
				(return res)
			)
			(incf current)			
		)
		res
	)
)

; Функция перевода
(defun translate (fileDir)
	(filCloseAll)
	(let ((res "") (listLines ()) (currentList ()) (bracketsStack ()) (currentLine "") (temp NIL))
		; Открытие файла с исходным кодом
		(filOpen 'fi fileDir _InPut)

		; Чтение строк из файла
		(loop
			(setq currentLine (filgetline 'fi))
			(setq listLines (append listLines (list (split currentLine))))
			(if (fileof 'fi)
				(return 1)
			)
		)

		; Главный цикл обработки команд
		(iter (for i from 0 to (- (length listLines) 1))
			(setq currentList (nth i listLines)) ; Запоминание текущей строки
			(printline currentList)
			(cond 
				; Если строка начинается на "if" ==========
				((eq "if" (nth 0 currentList))
					; push скобок в стэк
					(push "-1" bracketsStack)
					(push (strcat ")" (strchr 10)) bracketsStack)
					(setq temp (subseq currentList 2 (- (length currentList) 2)))
					(setq res (strcat res " (if "  (translateExp temp) (strchr 10)))
					; push скобок в стэк
					(push "-1" bracketsStack)
					(push (strcat ")" (strchr 10)) bracketsStack)
					(setq res (strcat res " (progn " (strchr 10)))
				)

				; Если строка начинается на "endif" ==========
				((eq "end_if" (nth 0 currentList))
					; Добавление скобок
					(loop
						(setq res (strcat res (pop bracketsStack)))
						(when (eq "-1" (nth 0 bracketsStack))
							(pop bracketsStack)
							(return 1)
						)
					)
					; Второе добавление скобок
					(loop
						(setq res (strcat res (pop bracketsStack)))
						(when (eq "-1" (nth 0 bracketsStack))
							(pop bracketsStack)
							(return 1)
						)
					)
				)

				; Если строка начинается на else ==========
				((eq "else" (nth 0 currentList))
					(loop
						(setq res (strcat res (pop bracketsStack)))
						(when (eq "-1" (nth (- (length bracketsStack) 1) bracketsStack))
							(pop bracketsStack)
							(return 1)
						)
					)
					; push скобок в стэк
					(push "-1" bracketsStack)
					(push (strcat ")" (strchr 10)) bracketsStack)
					(setq res (strcat res " (progn " (strchr 10)))
				)

				; Если строка начинается на for ==========
				((eq "for" (nth 0 currentList))
					; push скобок в стэк
					(push "-1" bracketsStack)
					(push (strcat ")" (strchr 10)) bracketsStack)

					(setq res (strcat res " (iter (for "))

					(setq temp (eq "step" (nth (- (length currentList) 2) currentList)))

					; Добавление итератора
					(setq res (strcat res (nth 1 currentList)))
					; Добавление начального значения
					(setq res (strcat res " from "))
					(setq res (strcat res (translateExp (subseq currentList 3 (position "to" currentList)))))
					(setq res (strcat res " to "))

					; Проверка step
					(if (and temp (null (eq "1" (nth (- (length currentList) 1) currentList))))
						; Если есть step и он не 1
						(progn
							; Добавление конечного значения
							(setq res (strcat res (translateExp (subseq currentList (+ 1 (position "to" currentList)) (position "step" currentList)))))
							; Добавление шага и закрывающей скобки
							(setq res (strcat res " by " (nth (- (length currentList) 1) currentList)))
						)
						; Если нет step или он равен 1
						(progn
							; Добавление конечного значения
							(if temp
								; Если есть step и он равен 1
								(setq res (strcat res (translateExp (subseq currentList (+ 1 (position "to" currentList)) (position "step" currentList)))))
								; Если step отсутствует
								(setq res (strcat res (translateExp (subseq currentList (+ 1 (position "to" currentList))))))
							)
							
						)
					)
					; Добавление закрывающей скобки
					(setq res (strcat res ")" (strchr 10)))
				)

				; Если строка начинается на end_for
				((eq "end_for" (nth 0 currentList))
					; Добавление скобок
					(loop
						(setq res (strcat res (pop bracketsStack)))
						(when (eq "-1" (nth 0 bracketsStack))
							(pop bracketsStack)
							(return 1)
						)
					)
				)

				; Если строка начинается на proc ==========
				((eq "proc" (nth 0 currentList))
					(setq variables nil)					
                    ;push скобок в стэк 
                    (dolist (curLine listLines t)
                    	(when (eq (nth 0 curLine) "local")
                            (setq variables (append variables (cdr curLine)))                        	                          
                      	)
                    ) 

                    (push "-1" bracketsStack)
					(push (strcat ")" (strchr 10)) bracketsStack)

					(setq funcName (nth 1 currentList))

                   	(setq funcArguments (subseq currentList (+ 1 (position "{" currentList )) (position "}" currentList) ))

					(setq res (strcat res " (defun "  FuncName (strrep (output funcArguments) """" "")  (strchr 10)))

					; Для LET
 	                (setq variables (mapcar 'input variables))
					(when (null(null variables)) 
						(setq res (strcat res " (let (result nil) " (output(createListOfVariables variables) ")") 
						(strchr 10)))
						(push (strcat ")" (strchr 10)) bracketsStack)
					)
					
				)

				; Если строка наинается на end_proc
				((eq "end_proc" (nth 0 currentList))
					(loop
						(setq res (strcat res (pop bracketsStack)))
						(when (eq "-1" (nth 0 bracketsStack))
							(pop bracketsStack)
							(return 1)
						)
					)
				)

				; Если строка начинается на print
				((eq "print" (nth 0 currentList))
					(let (printExp nil)
						(setq printExp (subseq currentList 1)) ; Выражение для печати
						(cond
							; Если на вывод одна переменная/цифра
							((= 1 (length printExp)) 
								(setq res (strcat res " (print" (nth currentList) ")"))
							)

							; Если на вывод 

						)
					)
				)
			)
		)
		(printline res)
	)
	(filClose 'fi) ; Закрытие файла с исходным кодом
)
