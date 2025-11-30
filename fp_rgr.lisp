;;; Розрахунково-графічна робота
;;; Варіант 8: Модифікована послідовність Фібоначчі

(defun fibonacci-variant8 (n)
  "Calculates the nth element of the modified Fibonacci sequence.
   F1 = 1, F10 = 1
   Fi = 2*F(i-1) - cos(i), i = 2..9
   Fi = 5*F(i-1) + sin(i), i = 11..20"
  (cond
    ;; Базові випадки
    ((= n 1) 1.0)
    ((= n 10) 1.0)
    
    ;; Для i = 2..9: Fi = 2*F(i-1) - cos(i)
    ((and (>= n 2) (<= n 9))
     (- (* 2 (fibonacci-variant8 (- n 1))) 
        (cos n)))
    
    ;; Для i = 11..20: Fi = 5*F(i-1) + sin(i)
    ((and (>= n 11) (<= n 20))
     (+ (* 5 (fibonacci-variant8 (- n 1))) 
        (sin n)))
    
    ;; Поза діапазоном
    (t (error "n must be in range 1..20"))))

;;; Оптимізована версія з мемоізацією
(defun fibonacci-variant8-memo ()
  "Створює мемоізовану версію функції для ефективних обчислень"
  (let ((cache (make-hash-table)))
    ;; Заповнюємо базові значення
    (setf (gethash 1 cache) 1.0)
    (setf (gethash 10 cache) 1.0)
    
    (lambda (n)
      (or (gethash n cache)
          (setf (gethash n cache)
                (cond
                  ((= n 1) 1.0)
                  ((= n 10) 1.0)
                  ((and (>= n 2) (<= n 9))
                   (- (* 2 (funcall #'fibonacci-variant8-memo-helper 
                                   (- n 1) cache))
                      (cos n)))
                  ((and (>= n 11) (<= n 20))
                   (+ (* 5 (funcall #'fibonacci-variant8-memo-helper 
                                   (- n 1) cache))
                      (sin n)))
                  (t (error "n must be in range 1..20"))))))))

(defun fibonacci-variant8-memo-helper (n cache)
  "Допоміжна функція для мемоізації"
  (or (gethash n cache)
      (setf (gethash n cache)
            (fibonacci-variant8 n))))

;;; ТЕСТУВАННЯ

(defun test-fibonacci ()
  "Тестує реалізацію послідовності"
  (format t "~%===== TESTING VAR 8 =====~%~%")
  
  ;; Обчислюємо всю послідовність
  (format t "Recursive:~%")
  (loop for i from 1 to 20 do
        (format t "F~2D = ~,6F~%" i (fibonacci-variant8 i)))

  ;; Перевірка специфічних значень
  (format t "~%Key values test:~%")
  (format t "F1 = ~A (awaiting: 1.0)~%" (fibonacci-variant8 1))
  (format t "F10 = ~A (awaiting: 1.0)~%" (fibonacci-variant8 10))
  
  ;; Перевірка формул
  (format t "~%Formula test:~%")
  (let ((f2-calc (- (* 2 (fibonacci-variant8 1)) (cos 2)))
        (f2-actual (fibonacci-variant8 2)))
    (format t "F2: 2*F1 - cos(2) = ~,6F = ~,6F~%" f2-calc f2-actual))
  
  (let ((f11-calc (+ (* 5 (fibonacci-variant8 10)) (sin 11)))
        (f11-actual (fibonacci-variant8 11)))
    (format t "F11: 5*F10 + sin(11) = ~,6F = ~,6F~%" f11-calc f11-actual)))

;;; Запуск всіх тестів
(defun run-all-tests ()
  "Запускає всі тести та виводить результати"
  (test-fibonacci))

;; Виконання
(run-all-tests)

; F1  = 1.000000
; F2  = 2.416147  (2*1 - cos(2))
; F3  = 5.822140  (2*2.416147 - cos(3))

; F10 = 1.000000
; F11 = 4.000099  (5*1 + sin(11))
; F12 = 19.463596 (5*4.000099 + sin(12))
