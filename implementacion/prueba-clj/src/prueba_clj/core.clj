(ns prueba-clj.core)

;Leer los datos
(defn leer-datos [archivo]
  (with-open [rdr (java.io.BufferedReader. (java.io.FileReader. archivo))]
    (read-string (slurp rdr))))

;Guardar el estado en archivo
(defn guardar-estado [estado]
  (spit "estado_actual.txt" estado))

;Crear la matriz
(def almacen (leer-datos "almacen_datos.txt"))

(defn crear-matriz []
  almacen)

;Calcular número de filas y columnas
(defn calcular-numero-de-filas [matriz]
  (count matriz))

(defn calcular-numero-de-columnas [matriz]
  (if (empty? matriz)
    0
    (count (first matriz))))

;Iniciar en estado actual
(do (guardar-estado '(0 0)))

;Estructura para los carruseles
(defn crear-carrusel [id almacen]
  {id {:almacen almacen :estado-actual [0 0]}})

(defn agregar-carrusel [carruseles id carrusel]
  (assoc carruseles id carrusel))

(defn inicializar-carruseles [ids archivos-almacen]
  (reduce-kv (fn [carruseles id archivo]
               (agregar-carrusel carruseles id (crear-carrusel id (leer-datos archivo))))
             {}
             ids))



;Movimiento
(defn mover-carrusel [carruseles carrusel-id movimiento]
   (let [carrusel (get carruseles carrusel-id)]
     (if carrusel
       (let [inventario (:almacen carrusel)
             fila-actual (:estado-actual carrusel 0)
             columna-actual (:estado-actual carrusel 1)
             num-filas (count inventario)
             num-columnas (if (empty? inventario) 0 (count (first inventario)))]
         (condp = movimiento
           :arriba
           (if (> fila-actual 0)
             (assoc-in carruseles [carrusel-id :estado-actual 0] (dec fila-actual))
             (do
               (println "Movimiento inválido: el carrusel no puede moverse hacia arriba")
               carruseles))
           :abajo
           (if (< fila-actual (dec num-filas))
             (assoc-in carruseles [carrusel-id :estado-actual 0] (inc fila-actual))
             (do
               (println "Movimiento inválido: el carrusel no puede moverse hacia abajo")
               carruseles))
           :izquierda
           (if (> columna-actual 0)
             (assoc-in carruseles [carrusel-id :estado-actual 1] (dec columna-actual))
             (do
               (println "Movimiento inválido: el carrusel no puede moverse hacia la izquierda")
               carruseles))
           :derecha
           (if (< columna-actual (dec num-columnas))
             (assoc-in carruseles [carrusel-id :estado-actual 1] (inc columna-actual))
             (do
               (println "Movimiento inválido: el carrusel no puede moverse hacia la derecha")
               carruseles))
           :default
           (do
             (println "Movimiento desconocido")
             carruseles))))
     (do
       (println "Carrusel no encontrado")
       carruseles)))







;Obtener elemento
(defn obtener-elemento [archivo fila]
  (try
    (with-open [rdr (java.io.BufferedReader. (java.io.FileReader. archivo))]
      (let [lineas (doall (line-seq rdr))]
        (if (>= fila (count lineas))
          (throw (Exception. "Fila no encontrada en el archivo"))
          (nth lineas fila))))
    (catch Exception e
      (println "Error al leer el archivo:" (.getMessage e))
      nil)))


;Realizar movimientos
(defn realizar-movimiento [entrada]
  (let [estado-actual (leer-datos "estado_actual.txt")]
    (mover estado-actual entrada)
    (println "Producto en ventanilla:")
    (println (obtener-elemento "productos.txt" (second estado-actual)))))

;Mostrar ventanilla
(defn mostrar-ventanilla []
  (let [estado (leer-datos "estado_actual.txt")
        fila (if (and (sequential? estado) (> (count estado) 1)) (second estado) (throw (Exception. "Datos inesperados en estado_actual.txt")))
        columna (if (sequential? estado) (first estado) (throw (Exception. "Datos inesperados en estado_actual.txt")))
        producto (obtener-elemento "productos.txt" fila)]
    (println "Estado actual - Fila:" fila ", Columna:" columna)
    (println "Producto en ventanilla:" producto)))

;Buscar producto
(defn buscar-producto [matriz valor]
  (letfn [(buscar-item [lista col]
            (when-let [[prim & rest] lista]
              (if (= prim valor)
                [col]
                (buscar-item rest (inc col)))))
          (buscar-fila [fila row]
            (when-let [[prim & rest] fila]
              (or (when-let [result (buscar-item prim 0)]
                    (conj result row))
                  (buscar-fila rest (inc row)))))]
    (buscar-fila matriz 0)))

;Modificar elemento en lista y matriz
(defn modificar-elemento-lista [lista indice nuevo-valor]
  (loop [lst lista idx indice]
    (cond
      (empty? lst) nil
      (zero? idx) (cons nuevo-valor (rest lst))
      :else (conj (recur (rest lst) (dec idx)) (first lst)))))

(defn modificar-elemento-matriz [matriz fila columna nuevo-valor]
  (loop [mtrx matriz f fila]
    (cond
      (empty? mtrx) nil
      (zero? f) (conj (modificar-elemento-lista (first mtrx) columna nuevo-valor) (rest mtrx))
      :else (conj (recur (rest mtrx) (dec f)) (first mtrx)))))

;Modificar almacen
(defn modificar-almacen [fila columna nuevo-valor]
  (let [matriz-modificada (modificar-elemento-matriz (crear-matriz) fila columna nuevo-valor)]
    (spit "almacen_datos.txt" matriz-modificada)))

;Opciones de movimiento
(defn opciones-de-movimiento [destino actual almacen]
  (let [num-filas (calcular-numero-de-filas almacen)
        num-columnas (calcular-numero-de-columnas almacen)]

    (letfn [(opciones-cadr [dest-actual actual-actual]
              (cond
                (= dest-actual actual-actual) []
                (< dest-actual actual-actual) (cons 'a (opciones-cadr (dec dest-actual) actual-actual))
                (> dest-actual actual-actual) (cons 'b (opciones-cadr (inc dest-actual) actual-actual))
                :else (throw (Exception. "Error en opciones-cadr"))))
            (opciones-car [dest-actual actual-actual]
              (cond
                (= dest-actual actual-actual) []
                (< dest-actual actual-actual) (cons 'i (opciones-car (dec dest-actual) actual-actual))
                (> dest-actual actual-actual) (cons 'd (opciones-car (inc dest-actual) actual-actual))
                :else (throw (Exception. "Error en opciones-car"))))]

      (concat (opciones-car (first destino) (first actual))
              (opciones-cadr (second destino) (second actual)))))


;Procesar comandos
(defn procesar-comandos [estado-actual comandos almacen-datos]
  (reduce (fn [estado cmd]
            (mover estado [cmd] almacen-datos))
          estado-actual
          comandos))

; Función para leer transacciones desde un archivo
(defn leer-transacciones [archivo]
  (with-open [rdr (java.io.BufferedReader. (java.io.FileReader. archivo))]
    (doall (line-seq rdr))))

; Función para procesar una transacción
(defn procesar-transaccion [carruseles transaccion]
  (let [carrusel-id (:carrusel-id transaccion)
        tipo (:tipo transaccion)
        carrusel (get carruseles carrusel-id)]
    (cond
      (= tipo :mover) (mover-carrusel carrusel (:movimiento transaccion))
      (= tipo :agregar) (agregar-producto-carrusel carrusel (:producto transaccion) (:cantidad transaccion))
      (= tipo :retirar) (retirar-producto-carrusel carrusel (:producto transaccion) (:cantidad transaccion))
      :else (throw (Exception. "Tipo de transacción desconocido")))))

; Función para procesar todas las transacciones desde un archivo
(defn procesar-transacciones-desde-archivo [carruseles archivo-transacciones]
  (let [transacciones (leer-transacciones archivo-transacciones)]
    (doseq [transaccion transacciones]
      (procesar-transaccion carruseles transaccion))))

;Paralelización de procesamiento de transacciones
(defn procesar-transacciones-en-paralelo [carruseles archivo-transacciones]
  (let [transacciones (leer-transacciones archivo-transacciones)]
    (pmap #(procesar-transaccion carruseles %) transacciones)))

;Calcular el valor del inventario
(defn valor-inventario-carrusel [carrusel]
  (reduce (fn [total fila]
            (+ total (reduce (fn [subtotal producto]
                               (+ subtotal (* (:cantidad producto) (:precio producto))))
                             0 fila)))
          0 (:almacen carrusel)))

(defn valor-total-inventario [carruseles]
  (reduce (fn [total [id carrusel]]
            (+ total (valor-inventario-carrusel carrusel)))
          0 carruseles))


;Funcion leer para leer los datos
(defn leer-datos-seguro [archivo]
  (try
    (leer-datos archivo)
    (catch Exception e
      (println "Error al leer el archivo:" (.getMessage e))
      nil)))

;Identificar el carrusel
(defn carruseles-con-resurtido [carruseles]
  (filter (fn [[_ carrusel-info]]
            (necesita-resurtido? carrusel-info))
          carruseles))


;Agregar producto
(defn agregar-producto [producto cantidad almacen precio fila columna]
  (let [nuevo-almacen (+ almacen cantidad)]
    (modificar-almacen fila columna [producto nuevo-almacen precio])
    (println "Producto en ventanilla:" (obtener-elemento "productos.txt" fila))
    (println "Stock actual del producto" producto "es de:" nuevo-almacen)))


;Función agregar
(defn agregar [& args]
  (let [almacen-datos (crear-matriz)
        estado-actual (leer-datos "estado_actual.txt")]
    (cond
      (empty? args) (throw (Exception. "Número incorrecto de parámetros"))
      (nil? (second args))
      (do
        (println "Se agregan" (first args) "unidades al producto")
        (let [producto (buscar-producto almacen-datos (second estado-actual))]
          (agregar-producto (first producto)
                            (first args)
                            (second producto)
                            (nth producto 2)
                            (second estado-actual)
                            (first estado-actual)
                            (+ (second producto) (first args)))))
      :else
      (do
        (println "Movimientos:")
        (let [producto (buscar-producto almacen-datos (first args))]
          (agregar-producto (first args)
                            (second args)
                            (second producto)
                            (nth producto 2)
                            (second producto)
                            (first producto)
                            (+ (second producto) (second args))))
        (guardar-estado (buscar-producto almacen-datos (first args)))))))

(defn mover-carrusel [carruseles carrusel-id movimiento]
       (let [carrusel (get carruseles carrusel-id)]
         (if carrusel
           (let [inventario (:inventario carrusel)
                 fila-actual (:fila carrusel)
                 columna-actual (:columna carrusel)
                 num-filas (count inventario)
                 num-columnas (if (empty? inventario) 0 (count (first inventario)))]
             (condp = movimiento
               :arriba
               (if (> fila-actual 0)
                 (assoc-in carruseles [carrusel-id :fila] (dec fila-actual))
                 (println "Movimiento inválido: el carrusel no puede moverse hacia arriba")))
             :abajo
             (if (< fila-actual (dec num-filas))
               (assoc-in carruseles [carrusel-id :fila] (inc fila-actual))
               (println "Movimiento inválido: el carrusel no puede moverse hacia abajo")))
           :izquierda
           (if (> columna-actual 0)
             (assoc-in carruseles [carrusel-id :columna] (dec columna-actual))
             (println "Movimiento inválido: el carrusel no puede moverse hacia la izquierda")))
         :derecha
         (if (< columna-actual (dec num-columnas))
           (assoc-in carruseles [carrusel-id :columna] (inc columna-actual))
           (println "Movimiento inválido: el carrusel no puede moverse hacia la derecha")))
       :default (println "Movimiento desconocido"))))
   (println "Carrusel no encontrado"))))

(defn top-carruseles [carruseles]
  (->> carruseles
       (map (fn [id carrusel] [id (valor-inventario-carrusel carrusel)]))
       (sort-by second >)
       (take (max 1 (int (* 0.10 (count carruseles)))))))

(defn carruseles-con-resurtido [carruseles punto-reorden]
  (filter (fn [[id carrusel]]
            (some (fn [fila] (some (fn [producto] (< (:cantidad producto) punto-reorden)) fila))
                  (:almacen carrusel)))
          carruseles))

;Mostrar ventanilla
(defn mostrar-ventanilla []
  (let [estado-actual (leer-datos "estado_actual.txt")
        fila (second estado-actual)
        columna (first estado-actual)
        producto (obtener-elemento "productos.txt" fila)]
    (println "Estado actual - Fila:" fila ", Columna:" columna)
    (println "Producto en ventanilla:" producto)))


(defn procesar-transaccion [carrusel transaccion]
  ;; Asumiendo que transaccion es un mapa con claves como :tipo, :producto, :cantidad, etc.
  (let [tipo (:tipo transaccion)]
    (cond
      (= tipo :mover)   (procesar-movimiento carrusel transaccion)
      (= tipo :agregar) (procesar-agregar carrusel transaccion)
      (= tipo :retirar) (procesar-retirar carrusel transaccion)
      :else (throw (Exception. "Tipo de transacción desconocido")))))

(defn procesar-movimiento [carrusel transaccion]
  ;; Implementa la lógica para mover el carrusel
  ;; Retorna el nuevo estado del carrusel
  )

(defn procesar-agregar [carrusel transaccion]
  ;; Implementa la lógica para agregar productos al carrusel
  ;; Retorna el nuevo estado del carrusel
  )

(defn procesar-retirar [carrusel transaccion]
  ;; Implementa la lógica para retirar productos del carrusel
  ;; Retorna el nuevo estado del carrusel
  )

(defn procesar-transacciones-en-paralelo [carruseles transacciones]
  (let [resultados (pmap (fn [transaccion]
                           (let [carrusel-id (:carrusel-id transaccion)
                                 carrusel (get carruseles carrusel-id)]
                             [carrusel-id (procesar-transaccion carrusel transaccion)]))
                         transacciones)]
    ;; Procesa los resultados para actualizar el estado global de carruseles si es necesario
    ;; Por ejemplo, puedes usar un loop para actualizar cada carrusel con su nuevo estado
    ))



;Función retirar producto
(defn retirar-producto [producto cantidad almacen precio fila columna]
  (cond
    (= almacen 0)
    (println "ERROR: No hay producto en el almacén")
    (>= cantidad almacen)
    (do
      (modificar-almacen fila columna [producto 0 precio])
      (println "Producto en ventanilla:" (obtener-elemento "productos.txt" fila))
      (println "Cantidad retirada:" almacen)
      (println "Almacenamiento actual: 0"))
    :else
    (let [nuevo-almacen (- almacen cantidad)]
      (modificar-almacen fila columna [producto nuevo-almacen precio])
      (println "Producto en ventanilla:" (obtener-elemento "productos.txt" fila))
      (println "Cantidad restante en el almacén del producto" producto "es de:" nuevo-almacen))))

;Funcion retirar
(defn retirar [& args]
  (let [almacen-datos (crear-matriz)
        estado-actual (leer-datos "estado_actual.txt")]
    (cond
      (empty? args) (throw (Exception. "Número incorrecto de parámetros"))
      (nil? (second args))
      (do
        (println "Se retiran" (first args) "unidades del producto")
        (let [producto (buscar-producto almacen-datos (second estado-actual))]
          (retirar-producto (first producto)
                            (first args)
                            (second producto)
                            (nth producto 2)
                            (second estado-actual)
                            (first estado-actual))))
      :else
      (do
        (println "Movimiento:")
        (println (concat (opciones-car (buscar-producto almacen-datos (first args)) estado-actual)
                         (opciones-cadr (buscar-producto almacen-datos (first args)) estado-actual)))
        (let [producto (buscar-producto almacen-datos (first args))]
          (retirar-producto (first args)
                            (second args)
                            (second producto)
                            (nth producto 2)
                            (second producto)
                            (first producto)))
        (guardar-estado (buscar-producto almacen-datos (first args)))))))

;Funcion ver estado actual de almacen
(defn ver-estado-almacen []
  (println "Estado actual del almacén:")
  (doseq [fila (crear-matriz)]
    (doseq [elemento fila]
      (print elemento " "))
    (println)))

(use 'clojure.test)

(deftest test-cargar-datos
  (testing "Prueba de cargar datos desde un archivo"
    (is (= ["dato1" "dato2" "dato3"] (cargar-datos "/Users/arturosanchez/desktop/implementacion/prueba-clj/transacciones.txt")))))


; ---------Prueba---------
(println "-------------PRUEBAS------------")

; Test cargar-datos
(println (cargar-datos "almacen_datos.txt")) ; Este ya lo tenías

; Test leer-datos (asumiendo que ya existe el archivo almacen_datos.txt)
(println (leer-datos "almacen_datos.txt"))

; Test crear-matriz (asumiendo que ya has leído los datos previamente en la variable `almacen`)
(println (crear-matriz))

; Test calcular-numero-de-filas
(println (calcular-numero-de-filas [[1 2 3] [4 5 6] [7 8 9]]))

; Test calcular-numero-de-columnas
(println (calcular-numero-de-columnas [[1 2 3] [4 5 6] [7 8 9]]))

; Test guardar-estado (verifica manualmente el archivo "estado_actual.txt" después de ejecutar)
(guardar-estado [2 2])

; Test obtener-elemento
(println (obtener-elemento "productos.txt" 2))

; Test mostrar-ventanilla
(mostrar-ventanilla)

; Test buscar-producto (asumiendo que hay una matriz 3x3 con valores de 1 a 9 en almacen_datos.txt)
(println (buscar-producto [[1 2 3] [4 5 6] [7 8 9]] 6))

; Test sumar
(println (sumar 5 6))

; Test agregar-producto
(agregar-producto "Producto1" 10 5 100 1 1 (sumar 5 10))

; Test modificar-almacen
(modificar-almacen 1 1 "NuevoProducto")

; Test opciones-cadr y opciones-car (estas son un poco más complejas, así que solo te doy un ejemplo simple)
(println (opciones-cadr [1 1] [0 0]))
(println (opciones-car [1 1] [0 0]))

; Test restar
(println (restar 8 5))

; Ahora, pruebas que deberían generar errores:

; Test cargar-datos con archivo inexistente
; (println (cargar-datos "archivo_inexistente.txt"))

; Test leer-datos con archivo inexistente
; (println (leer-datos "archivo_inexistente.txt"))

; Test obtener-elemento con fila que no existe
; (println (obtener-elemento "productos.txt" 1000))

; Test agregar con menos argumentos
; (agregar)

; Test agregar con producto que no existe
; (agregar "ProductoNoExistente" 5)
