(ns main
  (:use clojure.contrib.server-socket
    clojure.contrib.str-utils
    database))

(import '[java.io BufferedReader InputStreamReader OutputStreamWriter])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fake database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; definition of a session, right now it just contains an inbox for messages to process
(defstruct session :inbox)

;; holds session data. user id -> session
(def sessions (ref {}))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login server functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn login-user
  "takes in a user name string and trys to login the user. Returns the user's id or nil if the user could not be logged on"
  [user-name password]
  (let [user          (with-db (find-user-with-name user-name)) ; get the user data from the "database"
        is-empty?     (empty? user-name)                        ; blanks user names are a source of trouble
        is-logged-in  (contains? @sessions (:id user))          ; check if the user is already logged in
        correct-pass? (= (:password user) password)             ; check the password
        valid-login   (not (or is-empty? is-logged-in (not correct-pass?)))]
    (if valid-login
      (do (dosync (alter sessions assoc (:id user) (struct session '())))
        (:id user))
      nil)))


(defn logoff-user
  "logs off a user given the user's id, returns nil"
  [user-id]
  (do (dosync (alter sessions dissoc user-id))
    nil))


(defn register-user
  "creates a new user, returns the true or false"
  [user-name password]
  (let [user      (with-db (find-user-with-name user-name))
        is-empty? (or (empty? user-name) (empty? password))]
    (if (or user is-empty?)
      false
      (do (with-db (add-user user-name password))
        true))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server basics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn send-msg
  "sends a message to the output desired"
  [msg out]
  (binding [*out* (OutputStreamWriter. out)]
    (println msg)
    (flush)))

(defn get-msg
  "gets a message from the input"
  [in]
  (binding [*in* (BufferedReader. (InputStreamReader. in))]
    (read-line)))

(defn process-msg
  "trys to process a message, returns the new user data"
  [msg user-data out]
  (let [data  (re-split #":" msg)
        func  (first data)
        args  (rest data)
        reply #(do (send-msg %1 out) %2)]
    (try
      (cond
        ; try to login
        (= "login" func)    (let [result (apply login-user args)]
                              (if (nil? result)
                                (reply "Invalid credentials" user-data)
                                (reply "Logged in" (assoc user-data :id result))))
        ;try to register a user
        (= "register" func) (let [result (apply register-user args)]
                              (if (nil? result)
                                (reply "User already exists." user-data)
                                (reply "Created new user"  user-data)))
        ; we have no idea what message they are trying to do
        :else             (reply "Invalid message" user-data))
      (catch Exception e  (do (println e)
                            (reply "Error while processing message" user-data))))))

(defn server-loop
  "called when a new connection is added. It's the main loop for the connection"
  [in out]
  (println "Got connection.")
  (loop [user-data {:id 0}]
    (let [input  (get-msg in)]
      (cond 
        (nil? input)  (logoff-user (:id user-data))                 ; user disconnected, log them off
        :else         (recur (process-msg input user-data out)))))  ; valid message, try and process it
  (println "User Disconnected."))

(create-server 7777 server-loop)