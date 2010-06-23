(ns database
  (:use clojure.contrib.sql))

(def db {:classname "org.sqlite.JDBC"
         :subname "server.db"
         :subprotocol "sqlite"
         :create true})


;; users database

(defn create-users
  "Creates a default users table"
  []
  (create-table
   :users
   [:id :int "PRIMARY KEY"]
   [:name "varchar(32)"]
   [:password "varchar(32)"]))

(defn drop-users
  "deletes the users table"
  []
  (try
   (drop-table :users)
   (catch Exception _)))

(defn next-user-id
  "get the next available user id"
  []
  (with-query-results res
    ["SELECT id FROM users ORDER BY id DESC limit 1"]
    (inc (:id (first res)))))

(defn add-user
  "adds a user"
  [user-name password]
  (insert-rows
    :users
    [(next-user-id) user-name password]))

(defn add-admin-user
  "Adds the admin user"
  []
  (insert-rows
    :users
    [1 "admin" "password"]))

(defn find-user
  "gets a user given it's id"
  [id]
  (with-query-results res
      [(str "SELECT * FROM users WHERE id=" id)]
      (first res)))

(defn find-user-with-name
  "gets a user given it's name"
  [name]
  (with-query-results res
      [(str "SELECT * FROM users WHERE name='" name "'")]
      (first res)))


;; general functions

(defmacro with-db
  "performs a function with a connection and the default database"
  [func & args]
  `(with-connection db 
     (transaction
       ~func ~@args)))


;; setup database

(with-connection db
    (transaction
     (drop-users)
     (create-users)
     (add-admin-user)))
