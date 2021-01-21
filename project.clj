(ns idiot
  (:import (java.security MessageDigest)
           (java.io ByteArrayOutputStream ByteArrayInputStream)
           (java.util.zip DeflaterOutputStream InflaterInputStream))
  (:require [clojure.java.io :as io]))

(use '[clojure.string :only [index-of]])

(def argList ())
(def workingDirectory "./")
(def idiotDirectory ".idiot/")
;; Note: you must import java.security.MessageDigest for this to work.
;; E.g. include `(:import java.security.MessageDigest)` in your `ns` form.
(defn sha1-hash-bytes [data]
  (.digest (MessageDigest/getInstance "sha1")
           (.getBytes data)))

(defn byte->hex-digits [byte]
  (format "%02x"
          (bit-and 0xff byte)))

(defn bytes->hex-string [bytes]
  (->> bytes
       (map byte->hex-digits)
       (apply str)))

(defn sha1-sum [header+blob]
  (bytes->hex-string (sha1-hash-bytes header+blob)))


;; Note: this code assumes that `clojure.java.io` has been required as `io`.
;; E.g. include `(:require [clojure.java.io :as io])` to your `ns` form. It
;; also assumes that the `java.util.zip.DeflaterOutputStream`,
;; `java.io.ByteArrayOutputStream`, and `java.io.ByteArrayInputStream` classes
;; have been imported, e.g. by adding this statement to your `ns` form:
;; `(:import java.util.zip.DeflaterOutputStream
;;           (java.io ByteArrayInputStream
;;                    ByteArrayOutputStream))`


(defn zip-str
  "Zip the given data with zlib. Return a ByteArrayInputStream of the zipped
  content."
  [data]
  (let [out (ByteArrayOutputStream.)
        zipper (DeflaterOutputStream. out)]
    (io/copy data zipper)
    (.close zipper)
    (ByteArrayInputStream. (.toByteArray out))))

;; Note: this code assumes that `clojure.java.io` has been required as `io`
;; and that the java.io.ByteArrayOutputStream and
;; java.util.zip.InflaterInputStream classes have been imported.
;; For example:
;; (ns ,,,  ; your namespace name goes here
;;   (:require [clojure.java.io :as io])
;;   (:import java.io.ByteArrayOutputStream
;;            java.util.zip.InflaterInputStream))
(defn unzip
  "Unzip the given data with zlib. Pass an opened input stream as the arg. The
  caller should close the stream afterwards."
  [input-stream]
  (with-open [unzipper (InflaterInputStream. input-stream)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (->> (.toByteArray out)
         (map char)
         (apply str))))

;; Added for Assignment 3

(defn unzip-binary
  "Unzip the given file's contents with zlib."
  [path]
  (with-open [input (-> path io/file io/input-stream)
              unzipper (InflaterInputStream. input)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (.toByteArray out)))

;; Note that if given binary data this will fail with an error message
;; like:
;; Execution error (IllegalArgumentException) at ,,,.
;; Value out of range for char: -48
(defn bytes->str [bytes]
  (->> bytes (map char) (apply str)))

(defn split-at-byte [b bytes]
  (let [part1 (take-while (partial not= b) bytes)
        part2 (nthrest bytes (-> part1 count inc))]
    [part1 part2]))
(defn sha-bytes [bytes]
  (.digest (MessageDigest/getInstance "sha1") bytes))

(defn to-hex-string
  "Convert the given byte array into a hex string, 2 characters per byte."
  [bytes]
  (letfn [(to-hex [byte]
            (format "%02x" (bit-and 0xff byte)))]
    (->> bytes (map to-hex) (apply str))))

(defn hex-digits->byte
  [[dig1 dig2]]
  ;; This is tricky because something like "ab" is "out of range" for a
  ;; Byte, because Bytes are signed and can only be between -128 and 127
  ;; (inclusive). So we have to temporarily use an int to give us the room
  ;; we need, then adjust the value if needed to get it in the range for a
  ;; byte, and finally cast to a byte.
  (let [i (Integer/parseInt (str dig1 dig2) 16)
        byte-ready-int (if (< Byte/MAX_VALUE i)
                         (byte (- i 256))
                         i)]
    (byte byte-ready-int)))

(defn from-hex-string
  [hex-str]
  (byte-array (map hex-digits->byte (partition 2 hex-str))))

(defn getObjectTypeFromAddressString [oAddress]
  (def oDir (subs oAddress 0 2))
  (def oFile (subs oAddress 2))
  (def fullObject (unzip-binary (str workingDirectory idiotDirectory "objects/" oDir "/" oFile)))
  (def oHeader (bytes->str (nth (split-at-byte 0 fullObject) 0)))
  (subs oHeader 0 (index-of oHeader " ")))

(defn makeBlob [blobData]
  (def myBlob (str "blob " (count blobData) "\000" blobData))
  (def mySha1 (sha-bytes (.getBytes myBlob)))
  (def mySha1 (to-hex-string mySha1))
  (def myDir (subs mySha1 0 2))
  (.mkdir (java.io.File. (str workingDirectory idiotDirectory "objects/" myDir)))
  (def myFName (subs mySha1 2))
  (sha-bytes (.getBytes myBlob))                          ;return the 20 byte address
  )

(defn makeBlobWrite [blobData]
  (def myBlob (str "blob " (count blobData) "\000" blobData))
  (def mySha1 (sha-bytes (.getBytes myBlob)))
  (def mySha1 (to-hex-string mySha1))
  (def myDir (subs mySha1 0 2))
  (.mkdir (java.io.File. (str workingDirectory idiotDirectory "objects/" myDir)))
  (def myFName (subs mySha1 2))

  (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory "objects/" myDir "/" myFName))))
    (io/copy (zip-str myBlob) (io/file (str workingDirectory idiotDirectory "objects/" myDir "/" myFName)))
    ())

  ;(str mySha1)


  (sha-bytes (.getBytes myBlob))                            ;return the 20 byte address
  )

(defn helpFlag []
  (println "idiot: the other stupid content tracker\n\nUsage: idiot [<top-args>] <command> [<args>]\n\nTop-level arguments:\n   -r <dir>   run from the given directory instead of the current one\n   -d <dir>   store the database in <dir> (default: .idiot)\n\nCommands:\n   branch [-d <branch>]\n   cat-file {-p|-t} <address>\n   commit <tree> -m \"message\" [(-p parent)...]\n   commit-tree <tree> -m \"message\" [(-p parent)...]\n   hash-object [-w] <file>\n   help\n   init\n   rev-parse <ref>\n   switch [-c] <branch>\n   write-wtree"))

(defn help []
  (if (< (count argList) 2)
    (do
      (helpFlag)
      (System/exit 0))
    ())

  (if (or (= (nth argList 1) "-h") (= (nth argList 1) "--help"))
    (do
      (println "idiot help: print help for a command\n\nUsage: idiot help <command>\n\nArguments:\n   <command>   the command to print help for\n\nCommands:\n   branch [-d <branch>]\n   cat-file {-p|-t} <address>\n   commit <tree> -m \"message\" [(-p parent)...]\n   commit-tree <tree> -m \"message\" [(-p parent)...]\n   hash-object [-w] <file>\n   help\n   init\n   rev-parse <ref>\n   switch [-c] <branch>\n   write-wtree")
      (System/exit 0))
    ())

  (if (= (nth argList 1) "help")
    (do
      (println "idiot help: print help for a command\n\nUsage: idiot help <command>\n\nArguments:\n   <command>   the command to print help for\n\nCommands:\n   branch [-d <branch>]\n   cat-file {-p|-t} <address>\n   commit <tree> -m \"message\" [(-p parent)...]\n   commit-tree <tree> -m \"message\" [(-p parent)...]\n   hash-object [-w] <file>\n   help\n   init\n   rev-parse <ref>\n   switch [-c] <branch>\n   write-wtree")
      (System/exit 0))
    ())

  (if (= (nth argList 1) "init")
    (do
      (println "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message")
      (System/exit 0))
    ())

  (if (= (nth argList 1) "hash-object")
    (do
      (println "idiot hash-object: compute address and maybe create blob from file\n\nUsage: idiot hash-object [-w] <file>\n\nArguments:\n   -h       print this message\n   -w       write the file to database as a blob object\n   <file>   the file")
      (System/exit 0))
    ())

  (if (= (nth argList 1) "cat-file")
    (do
      (println "idiot cat-file: print information about an object\n\nUsage: idiot cat-file {-p|-t} <address>\n\nArguments:\n   -h          print this message\n   -p          pretty-print contents based on object type\n   -t          print the type of the given object\n   <address>   the SHA1-based address of the object")
      (System/exit 0))
    ())

  (if (= (nth argList 1) "write-wtree")
    (do
      (println "idiot write-wtree: write the working tree to the database\n\nUsage: idiot write-wtree\n\nArguments:\n   -h       print this message")
      (System/exit 0))
    ())

  (if (= (nth argList 1) "commit-tree")
    (do
      (println "idiot commit-tree: write a commit object based on the given tree\n\nUsage: idiot commit-tree <tree> -m \"message\" [(-p parent)...]\n\nArguments:\n   -h               print this message\n   <tree>           the address of the tree object to commit\n   -m \"<message>\"   the commit message\n   -p <parent>      the address of a parent commit")
      (System/exit 0))
    ())

  (if (= (nth argList 1) "rev-parse")
    (do
      (println "idiot rev-parse: determine which commit a ref points to\n\nUsage: idiot rev-parse <ref>\n\n<ref> can be:\n- a branch name, like 'master'\n- literally 'HEAD'\n- literally '@', an alias for 'HEAD'")
      (System/exit 0))
    ())

  (if (= (nth argList 1) "switch")
    (do
      (println "idiot switch: change what HEAD points to\n\nUsage: idiot switch [-c] <branch>\n\nArguments:\n   -c   create the branch before switching to it")
      (System/exit 0))
    ())

  (if (= (nth argList 1) "branch")
    (do
      (println "idiot branch: list or delete branches\n\nUsage: idiot branch [-d <branch>]\n\nArguments:\n   -d <branch>   delete branch <branch>")
      (System/exit 0))
    ())

  (if (= (nth argList 1) "commit")
    (do
      (println "idiot commit: create a commit and advance the current branch\n\nUsage: idiot commit <tree> -m \"message\" [(-p parent)...]\n\nArguments:\n   -h               print this message\n   <tree>           the address of the tree object to commit\n   -m \"<message>\"   the commit message\n   -p <parent>      the address of a parent commit")
      (System/exit 0))
    ())

  (println "Error: invalid command"))


;--------------------------------------------------------------------------------------
;  (if (and (> (count argList) 1) (or (= (nth argList 1) "-h") (= (nth argList 1) "--help")))


(defn init []
  (if (> (count argList) 1)
    (if (or (= (nth argList 1) "-h") (= (nth argList 1) "--help"))
      (println "idiot init: initialize a new database\n\nUsage: idiot init\n\nArguments:\n   -h   print this message")
      (println "Error: init accepts no arguments"))
    (if (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory)))
      (println "Error: .git directory already exists")

      (do
        (.mkdir (java.io.File. (str workingDirectory idiotDirectory)))
        (.mkdir (java.io.File. (str workingDirectory idiotDirectory "objects")))
        (.mkdir (java.io.File. (str workingDirectory idiotDirectory "refs")))
        (io/copy "ref: refs/heads/master\n" (io/file (str workingDirectory idiotDirectory "HEAD")))

        (println "Initialized empty Idiot repository in .git directory")))))

(defn hash-object []
  (if (< (count argList) 2)
    (do
      (println "Error: you must specify a file.")
      (System/exit 0))
    ())

  (if (and (< (count argList) 3) (= (nth argList 1) "-w"))
    (do
      (println "Error: you must specify a file.")
      (System/exit 0))
    ())

  (if (or (= (nth argList 1) "-h") (= (nth argList 1) "--help"))
    (do
      (println "idiot hash-object: compute address and maybe create blob from file\n\nUsage: idiot hash-object [-w] <file>\n\nArguments:\n   -h       print this message\n   -w       write the file to database as a blob object\n   <file>   the file")
      (System/exit 0))
    ())

  (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory))))
    (do
      (println "Error: could not find database. (Did you run `idiot init`?)")
      (System/exit 0))
    ())

  (if (and (>= (count argList) 3) (= (nth argList 1) "-w"))
    (do
      (try
        ;(println (slurp (str workingDirectory (nth argList 2))))
        (def myBlobData (slurp (str workingDirectory (nth argList 2))))
        (catch Exception e
          (println "Error: that file isn't readable")
          (System/exit 0)))

      (println (to-hex-string (makeBlobWrite myBlobData))))
    ())

  (if (and (>= (count argList) 2) (not (= (nth argList 1) "-w")))
    (do
      (try
        (do
          (def myBlobData (slurp (str workingDirectory (nth argList 1)))))
        (catch Exception e
          (println "Error: that file isn't readable")
          (System/exit 0)))

      (println (to-hex-string (makeBlob myBlobData))))
    ()))

(defn cat-file []

  (if (< (count argList) 2)
    (do
      (println "Error: the -p or -t switch is required")
      (System/exit 0))
    ())

  (if (or (= (nth argList 1) "-h") (= (nth argList 1) "--help"))
    (do
      (println "idiot cat-file: print information about an object\n\nUsage: idiot cat-file {-p|-t} <address>\n\nArguments:\n   -h          print this message\n   -p          pretty-print contents based on object type\n   -t          print the type of the given object\n   <address>   the SHA1-based address of the object")
      (System/exit 0))
    ())

  (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory))))
    (do
      (println "Error: could not find database. (Did you run `idiot init`?)")
      (System/exit 0))
    ())

  (if (not (or (= (nth argList 1) "-p") (= (nth argList 1) "-t")))
    (do
      (println "Error: the -p or -t switch is required")
      (System/exit 0))
    ())

  (if (< (count argList) 3)
    (do
      (println "Error: you must specify an address")
      (System/exit 0))
    ())

  (def myDir (subs (nth argList 2) 0 2))
  (def myFName (subs (nth argList 2) 2))

  (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory "objects/" myDir "/" myFName))))
    (do
      (println "Error: that address doesn't exist")
      (System/exit 0))
    ())

  (def blobby (unzip-binary (str workingDirectory idiotDirectory "objects/" myDir "/" myFName)))
  (def blobbyStr (bytes->str (nth (split-at-byte 0 blobby) 0)))
  (def objType (subs blobbyStr 0 (index-of blobbyStr " ")))

  (if (= (nth argList 1) "-t")
    (do
      (println objType)
      (System/exit 0))
    ())

  (if (= (nth argList 1) "-p")
    (do
      (if (= objType "commit")
        (do
          (def blobbyStr2 (bytes->str (nth (split-at-byte 0 blobby) 1)))
          (print blobbyStr2)
          (flush))
        ())
      (if (= objType "blob")
        (do
          (def blobbyStr2 (bytes->str (nth (split-at-byte 0 blobby) 1)))
          (print blobbyStr2)
          (flush))
        ())
      (if (= objType "tree")
        (do
          (def treeInfo (split-at-byte 0 (nth (split-at-byte 0 blobby) 1)))
          (def treeHeader2 (bytes->str (nth treeInfo 0)))
          (def treeMode (subs treeHeader2 0 (index-of treeHeader2 " ")))
          (def treeType (subs treeHeader2 (+ (index-of treeHeader2 " ") 1)))
          (def treeAddress (to-hex-string (nth treeInfo 1)))

          (if (= treeMode "40000")
            (def treeMode (str "0" treeMode))
            ())

          (println (str treeMode " " (getObjectTypeFromAddressString treeAddress) " " treeAddress "\t" treeType)))

        ())

      (System/exit 0))
    ())

  ; works for tree commit binary
  ;(def blobby (unzip-binary (str workingDirectory idiotDirectory "objects/" myDir "/" myFName)))

  ; works for blobs with old unzip
  ;(def blobby (with-open [input (-> (str workingDirectory idiotDirectory "objects/" myDir "/" myFName) io/file io/input-stream)]
  ;              (unzip input)))
  ;(print (subs blobby (+ (index-of blobby "\000") 1)))
  ;(println (bytes->str (nth (split-at-byte 0 blobby) 0)))
  ;(flush)
  ;(println (+ (index-of blobby "\000") 1) )
  )

(defn rev-parse []

  (if (and (> (count argList) 1) (or (= (nth argList 1) "-h") (= (nth argList 1) "--help")))
    (do
      (println "idiot rev-parse: determine which commit a ref points to\n\nUsage: idiot rev-parse <ref>\n\n<ref> can be:\n- a branch name, like 'master'\n- literally 'HEAD'\n- literally '@', an alias for 'HEAD'")
      (System/exit 0))
    ())

  (if (> (count argList) 2)
    (do
      (println "Error: you must specify a branch name and nothing else.")
      (System/exit 0))
    ())

  (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory))))
    (do
      (println "Error: could not find database. (Did you run `idiot init`?)")
      (System/exit 0))
    ())

  (if (or (= (nth argList 1) "HEAD") (= (nth argList 1) "@"))

    (do
      (if (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory "HEAD")))
        (if (= (subs (slurp (str workingDirectory idiotDirectory "HEAD")) 0 4) "ref:")
          (print (slurp (subs (slurp (str workingDirectory idiotDirectory "HEAD")) 5 (index-of (slurp (str workingDirectory idiotDirectory "HEAD")) "\n")))) ;; do i need to end the subs at the \n??
          (print (slurp (str workingDirectory idiotDirectory "HEAD"))))
        ())
      (System/exit 0))
    ())

  (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory "refs/heads/" (nth argList 1)))))
    (print (str "Error: could not find ref named " (nth argList 1) "\n"))
    (print (slurp (str workingDirectory idiotDirectory "refs/heads/" (nth argList 1)))))

  (System/exit 0))

(defn switch []

  (if (and (> (count argList) 1) (or (= (nth argList 1) "-h") (= (nth argList 1) "--help")))
    (do
      (println "idiot switch: change what HEAD points to\n\nUsage: idiot switch [-c] <branch>\n\nArguments:\n   -c   create the branch before switching to it")
      (System/exit 0))
    ())

  (if (< (count argList) 1)
    (do
      (println "Error: you must specify a branch name.")
      (System/exit 0))
    ())

  (if (or (and (not (= (nth argList 1) "-c")) (> (count argList) 1)) (> (count argList) 2))
    (do
      (println "Error: you may only specify one branch name.")
      (System/exit 0))
    ())

  (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory))))
    (do
      (println "Error: could not find database. (Did you run `idiot init`?)")
      (System/exit 0))
    ())

  (if (= (nth argList 1) "-c")
    (do
      (if (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory "refs/heads/" (nth argList 2))))
        (println "Error: a ref with that name already exists.")
        (do

          (io/copy (slurp (str workingDirectory idiotDirectory "HEAD")) (io/file (str workingDirectory idiotDirectory "refs/heads/" (nth argList 2))))
          (io/copy (str "ref: refs/heads/" (nth argList 2) "\n") (io/file (str workingDirectory idiotDirectory "HEAD")))))

      (System/exit 0))
    ())

  (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory "refs/heads/" (nth argList 1)))))
    (println "Error: no ref with that name exists.")
    (do
      (io/copy (str "ref: refs/heads/" (nth argList 1) "\n") (io/file (str workingDirectory idiotDirectory "HEAD")))
      (println (str "Switched to branch '" (nth argList 1) "'")))))

(defn branch []

  (if (and (> (count argList) 1) (or (= (nth argList 1) "-h") (= (nth argList 1) "--help")))
    (do
      (println "idiot branch: list or delete branches\n\nUsage: idiot branch [-d <branch>]\n\nArguments:\n   -d <branch>   delete branch <branch>")
      (System/exit 0))
    ())

  (if (and (= (nth argList 1) "-d") (< (count argList) 2))
    (do
      (println "Error: you must specify a branch name.")
      (System/exit 0))
    ())

  (if (and (not (= (nth argList 1) "-d")) (= (count argList) 2))
    (do
      (println "Error: invalid arguments.")
      (System/exit 0))
    ())

  (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory))))
    (do
      (println "Error: could not find database. (Did you run `idiot init`?)")
      (System/exit 0))
    ())

  (if (= (nth argList 1) "-d")
    (do
      (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory "refs/heads/" (nth argList 2)))))
        (do
          (println (str "Error: branch '" (nth argList 2) "' not found."))
          (System/exit 0))
        (if (= (slurp (str workingDirectory idiotDirectory "HEAD")) (str "ref: refs/heads/" (nth argList 2) "\n"))
          (do
            (println (str "Error: cannot delete checked-out branch '" (nth argList 2) "'."))
            (System/exit 0))
          (do
            (io/delete-file (str workingDirectory idiotDirectory "refs/heads/" (nth argList 2)))
            (println "Deleted branch " (nth argList 2) ".")
            (System/exit 0)))))

    ())

  (def filesArray (.listFiles (clojure.java.io/file (str workingDirectory idiotDirectory "refs/heads/"))))
  (def newf (sort filesArray))
  (doseq [n newf]
    (if (= (slurp (str workingDirectory idiotDirectory "HEAD")) (str "ref: refs/heads/" (.getName n) "\n"))
      (println (str "* " (.getName n)))
      (println (str "  " (.getName n))))))

(defn commit []

  (if (and (> (count argList) 1) (or (= (nth argList 1) "-h") (= (nth argList 1) "--help")))
    (do
      (println "idiot commit: create a commit and advance the current branch\n\nUsage: idiot commit <tree> -m \"message\" [(-p parent)...]\n\nArguments:\n   -h               print this message\n   <tree>           the address of the tree object to commit\n   -m \"<message>\"   the commit message\n   -p <parent>      the address of a parent commit")
      (System/exit 0))
    ())

  (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory))))
    (do
      (println "Error: could not find database. (Did you run `idiot init`?)")
      (System/exit 0))
    ())

  (if (< (count argList) 2)
    (do
      (println "Error: you must specify a tree address.")
      (System/exit 0))
    ())

  (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory "objects/" (subs (nth argList 1) 0 2) "/" (subs (nth argList 1) 2)))))
    (do
      (println "Error: no tree object exists at that address.")
      (System/exit 0))
    ())

  (if (not (= (getObjectTypeFromAddressString (nth argList 1)) "tree"))
    (do
      (println "Error: an object exists at that address, but it isn't a tree.")
      (System/exit 0))
    ())

  (if (< (count argList) 3)
    (do
      (println "Error: you must specify a message.")
      (System/exit 0))
    ())

  (if (< (count argList) 4)
    (do
      (println "Error: you must specify a message with the -m switch.")
      (System/exit 0))
    ())

  (if (and (= (count argList) 5) (= (nth argList 4) "-p"))
    (do
      (println "Error: you must specify a commit object with the -p switch.")
      (System/exit 0))
    ())

  (def commit-object (str "tree " (nth argList 1) "\n"))

  (if (>= (count argList) 6)
    (do

      (def argVal 5)

      (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory "objects/" (subs (nth argList argVal) 0 2) "/" (subs (nth argList argVal) 2)))))
        (do
          (println (str "Error: no commit object exists at address " (nth argList argVal) "."))
          (System/exit 0))
        ())

      (if (not (= (getObjectTypeFromAddressString (nth argList argVal)) "commit"))
        (do
          (println (str "Error: an object exists at address " (nth argList argVal) ", but it isn't a commit."))
          (System/exit 0))
        ())

      (while (< argVal (count argList))
        (def commit-object (str commit-object "parent " (nth argList argVal) "\n"))
        (def argVal (+ argVal 1))))

    ())

  (def commit-object (str commit-object "author Linus Torvalds <torvalds@transmeta.com> 1581997446 -0500\ncommitter Linus Torvalds <torvalds@transmeta.com> 1581997446 -0500\n\n" (nth argList 3) "\n"))
  (def commit-object (str "commit " (count commit-object) "\000" commit-object))
  ;(println commit-object)
  (def commit-addr (sha-bytes (.getBytes commit-object)))

  (def commit-addr2 (to-hex-string commit-addr))
  (def myDir (subs commit-addr2 0 2))
  (.mkdir (java.io.File. (str workingDirectory idiotDirectory "objects/" myDir)))
  (def myFName (subs commit-addr2 2))
  (io/copy (zip-str commit-object) (io/file (str workingDirectory idiotDirectory "objects/" myDir "/" myFName)))

  (println "Commit created.")

  (if (= (subs (slurp (str workingDirectory idiotDirectory "HEAD")) 0 4) "ref:")
    (do
      (io/copy commit-addr2 (io/file (subs (slurp (str workingDirectory idiotDirectory "HEAD")) 5 (index-of (slurp (str workingDirectory idiotDirectory "HEAD")) "\n"))))

      (println "Updated branch " (subs (slurp (str workingDirectory idiotDirectory "HEAD")) (+ 15 (count workingDirectory) (count idiotDirectory))  (index-of (slurp (str workingDirectory idiotDirectory "HEAD")) "\n")) ".")

      ;(if (not (.exists (slurp (str workingDirectory idiotDirectory "HEAD"))))
      ;  ()
      ;  ()
      ;  )
      )
    ()))

(defn write-wtree [curDir]


  ;(if (or (= (nth argList 1) "-h") (= (nth argList 1) "--help"))
  ;  (do
  ;    (println "idiot write-wtree: write the working tree to the database\n\nUsage: idiot write-wtree\n\nArguments:\n   -h       print this message\n")
  ;    (System/exit 0)
  ;    )
  ;  ()
  ;  )


  (if (and (> (count argList) 1) (or (= (nth argList 1) "-h") (= (nth argList 1) "--help")))
    (do
      (println "idiot write-wtree: write the working tree to the database\n\nUsage: idiot write-wtree\n\nArguments:\n   -h       print this message")
      (System/exit 0))
    ())

  (if (> (count argList) 1)
    (do
      (println "Error: write-wtree accepts no arguments")
      (System/exit 0))
    ())

  (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory))))
    (do
      (println "Error: could not find database. (Did you run `idiot init`?)")
      (System/exit 0))
    ())
  (if (and (= (count (.listFiles (clojure.java.io/file curDir))) 1) (= idiotDirectory (str (.getName (nth (.listFiles (clojure.java.io/file curDir)) 0)) "/")))
    (do
      (println "The directory was empty, so nothing was saved.")
      (System/exit 0))
    ())

  ;MAKE HELPER FUNCTION THAT TAKE

  ;get file objects in specified directory only and print names


  (def filesArray (.listFiles (clojure.java.io/file curDir)))
  ;append header for tree object here ("tree (count tree)null"), then use loop to create tree contents by concating all files/dirs ("MODE PATHnullBINARY ADDRESS")
  (def namesSorted (sort filesArray))
  (def myTreeContents "")                                      ;


  (doseq [n namesSorted]
    (if (not (.isHidden n))
      (if (.isDirectory n)
        (do
          (def childDirFiles (file-seq (clojure.java.io/file (str curDir (.getName n)))))
          (def noFiles true)
          (doseq [n childDirFiles]
            (if (.isFile n)
              (def noFiles false)
              ()))

          (if (= noFiles false)
            (def myTreeContents (concat myTreeContents (.getBytes (str "40000 " (.getName n) "\000")) (write-wtree (str curDir (.getName n) "/")))))
          ())

        (do

          (def myTreeContents (concat myTreeContents (.getBytes (str "100644 "  (.getName n) "\000")) (makeBlobWrite (slurp (str curDir (.getName n))))))))

      (;if hidden skip it
       )))

  (def myTree (byte-array (concat (.getBytes (str "tree " (count myTreeContents) "\000")) myTreeContents)))

  (if (> (count myTreeContents) 0)
    (do
      (def mySha1 (sha-bytes myTree))
      (def myDir (subs (to-hex-string mySha1) 0 2))
      (.mkdir (java.io.File. (str workingDirectory idiotDirectory "objects/" myDir)))
      (def myFName (subs (to-hex-string mySha1) 2))
      (io/copy (zip-str myTree) (io/file (str workingDirectory idiotDirectory "objects/" myDir "/" myFName))))
    ())

  (sha-bytes myTree)

  ;(def allFiles (file-seq (clojure.java.io/file ".")))
  ;(def noFiles true)
  ;(doseq [n allFiles]
  ;  (if (.isFile n)
  ;    (def noFiles false)
  ;    ()
  ;    )
  ;  )
  ;(if (= noFiles false)
  ;  (io/copy (zip-str myTree) (io/file (str workingDirectory idiotDirectory "objects/" myDir "/" myFName)))
  ;  ()
  ;  )





  ;(def directory (clojure.java.io/file "."))
  ;(def files
  ;  (for [file (file-seq directory)] (.getName file)))
  ;(def files (sort files))
  ;(println (take 10 files))

  ;print names of all files recursively
  ;(def allFiles (file-seq (clojure.java.io/file ".")))
  ;(def noFiles true)
  ;(doseq [n allFiles]
  ;  (println (.isFile n))
  ;
  ;  (if (.isFile n)
  ;    (def noFiles false)
  ;    ()
  ;    )
  ;  )
  )

(defn commit-tree []

  (if (and (> (count argList) 1) (or (= (nth argList 1) "-h") (= (nth argList 1) "--help")))
    (do
      (println "idiot commit-tree: write a commit object based on the given tree\n\nUsage: idiot commit-tree <tree> -m \"message\" [(-p parent)...]\n\nArguments:\n   -h               print this message\n   <tree>           the address of the tree object to commit\n   -m \"<message>\"   the commit message\n   -p <parent>      the address of a parent commit")
      (System/exit 0))
    ())

  (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory))))
    (do
      (println "Error: could not find database. (Did you run `idiot init`?)")
      (System/exit 0))
    ())

  (if (< (count argList) 2)
    (do
      (println "Error: you must specify a tree address.")
      (System/exit 0))
    ())

  (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory "objects/" (subs (nth argList 1) 0 2) "/" (subs (nth argList 1) 2)))))
    (do
      (println "Error: no tree object exists at that address.")
      (System/exit 0))
    ())

  (if (not (= (getObjectTypeFromAddressString (nth argList 1)) "tree"))
    (do
      (println "Error: an object exists at that address, but it isn't a tree.")
      (System/exit 0))
    ())

  (if (< (count argList) 3)
    (do
      (println "Error: you must specify a message.")
      (System/exit 0))
    ())

  (if (< (count argList) 4)
    (do
      (println "Error: you must specify a message with the -m switch.")
      (System/exit 0))
    ())

  (if (and (= (count argList) 5) (= (nth argList 4) "-p"))
    (do
      (println "Error: you must specify a commit object with the -p switch.")
      (System/exit 0))
    ())

  (def commit-object (str "tree " (nth argList 1) "\n"))

  (if (>= (count argList) 6)
    (do

      (def argVal 5)

      (if (not (.exists (clojure.java.io/as-file (str workingDirectory idiotDirectory "objects/" (subs (nth argList argVal) 0 2) "/" (subs (nth argList argVal) 2)))))
        (do
          (println (str "Error: no commit object exists at address " (nth argList argVal) "."))
          (System/exit 0))
        ())

      (if (not (= (getObjectTypeFromAddressString (nth argList argVal)) "commit"))
        (do
          (println (str "Error: an object exists at address " (nth argList argVal) ", but it isn't a commit."))
          (System/exit 0))
        ())

      (while (< argVal (count argList))
        (def commit-object (str commit-object "parent " (nth argList argVal) "\n"))
        (def argVal (+ argVal 1))))

    ())

  (def commit-object (str commit-object "author Linus Torvalds <torvalds@transmeta.com> 1581997446 -0500\ncommitter Linus Torvalds <torvalds@transmeta.com> 1581997446 -0500\n\n" (nth argList 3) "\n"))
  (def commit-object (str "commit " (count commit-object) "\000" commit-object))
  ;(println commit-object)
  (def commit-addr (sha-bytes (.getBytes commit-object)))

  (def commit-addr2 (to-hex-string commit-addr))
  (def myDir (subs commit-addr2 0 2))
  (.mkdir (java.io.File. (str workingDirectory idiotDirectory "objects/" myDir)))
  (def myFName (subs commit-addr2 2))
  (io/copy (zip-str commit-object) (io/file (str workingDirectory idiotDirectory "objects/" myDir "/" myFName)))

  (println (to-hex-string commit-addr)))

(defn -main [& args]
  (def argList args)

  (if (= (nth argList 0) "-r")
    (do
      (if (< (count argList) 2)
        (do
          (println "Error: the -r switch needs an argument")
          (System/exit 0))
        ())
      (if (not (.exists (clojure.java.io/as-file (nth argList 1))))
        (do
          (println "Error: the directory specified by -r does not exist")
          (System/exit 0))
        ())
      (def workingDirectory (str "./" (nth argList 1) "/"))
      (def argList (drop 2 argList))
      ;handle -d after -r condition
      (if (= (nth argList 0) "-d")
        (do
          (if (< (count argList) 2)
            (do
              (println "Error: the -d switch needs an argument")
              (System/exit 0))
            ())

          (def idiotDirectory (str "./" (nth argList 1) "/"))
          (def argList (drop 2 argList)))
        ()))

    ())

  (if (= (nth argList 0) "-d")
    (do
      (if (< (count argList) 2)
        (do
          (println "Error: the -d switch needs an argument")
          (System/exit 0))
        ())

      (def idiotDirectory (str "./" (nth argList 1) "/"))
      (def argList (drop 2 argList))
      ;handle -r after -d condition
      (if (= (nth argList 0) "-r")
        (do
          (if (< (count argList) 2)
            (do
              (println "Error: the -r switch needs an argument")
              (System/exit 0))
            ())

          (if (not (.exists (clojure.java.io/as-file (nth argList 1))))
            (do
              (println "Error: the directory specified by -r does not exist")
              (System/exit 0))
            ())
          (def workingDirectory (str "./" (nth argList 1) "/"))
          (def argList (drop 2 argList)))

        ()))

    ())

  (if (= (nth argList 0) "help")
    (do
      (help)
      (System/exit 0))
    ())

  (if (= (nth argList 0) "init")
    (do
      (init)
      (System/exit 0))
    ())

  (if (= (nth argList 0) "hash-object")
    (do
      (hash-object)
      (System/exit 0))
    ())

  (if (= (nth argList 0) "cat-file")
    (do
      (cat-file)
      (System/exit 0))
    ())

  (if (or (= (nth argList 0) "-h") (= (nth argList 0) "--help") (= (nth argList 0) nil))
    (do
      (helpFlag)
      (System/exit 0))
    ())

  (if (= (nth argList 0) "write-wtree")
    (do
      (println (to-hex-string (write-wtree workingDirectory)))
      (System/exit 0))
    ())

  (if (= (nth argList 0) "commit-tree")
    (do
      (commit-tree)
      (System/exit 0))
    ())

  (if (= (nth argList 0) "rev-parse")
    (do
      (rev-parse)
      (System/exit 0))
    ())

  (if (= (nth argList 0) "switch")
    (do
      (switch)
      (System/exit 0))
    ())

  (if (= (nth argList 0) "branch")
    (do
      (branch)
      (System/exit 0))
    ())

  (if (= (nth argList 0) "commit")
    (do
      (commit)
      (System/exit 0))
    ())

  (println "Error: invalid command"))




