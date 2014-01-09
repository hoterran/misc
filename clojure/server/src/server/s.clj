(ns server.s
	(:import (java.nio.channels Selector SocketChannel ServerSocketChannel SelectionKey)
	(java.net InetSocketAddress)
	(java.nio ByteBuffer)
	(java.io IOException)))

(declare reactor process-keys accept-channel read-channel)

(defn bind [^InetSocketAddress addr fcol]
  (let [selector (Selector/open)
        ssc      (ServerSocketChannel/open)
        ag  (agent selector)]
    (do
      (.configureBlocking ssc false)
      (.. ssc (socket) (bind addr 1000))
      (.register ssc selector SelectionKey/OP_ACCEPT)
      (send-off ag reactor fcol)
      ag)))

(defn- reactor [^Selector selector fcol]
  (let [sel (. selector select 1000)]
    (if (> sel 0)
      (let [sks (. selector selectedKeys)]
        (do 
          (dorun (map (partial process-keys selector fcol) sks))
          (.clear sks))))
    (recur selector fcol)))
  
(defn- process-keys [^Selector selector ^SelectionKey fcol sk]
  (try
    (cond 
      (.isAcceptable sk) (accept-channel sk  selector fcol)
      (.isReadable sk) (read-channel sk selector fcol)    
    )
    (catch Throwable e (.printStackTrace e))))

(defn- accept-channel [^SelectionKey sk ^Selector selector fcol]
   (let [^ServerSocketChannel ssc (. sk channel)
         ^SocketChannel sc (. ssc accept)
         created-fn (:created fcol)]
     (do 
       (.configureBlocking sc false) 
       (.register sc selector SelectionKey/OP_READ)
       (if created-fn
         (created-fn sc)))))

(defn- close-channel [^SelectionKey sk ^SocketChannel sc fcol]
  (let [closed-fn (:closed fcol)]
    (do 
       (.close sc)
       (.cancel sk)
       (if closed-fn 
         (closed-fn sc)))))
     
(defn-  read-channel [^SelectionKey sk ^Selector selector fcol]
   (let [^SocketChannel sc (. sk channel)
         ^ByteBuffer buf (ByteBuffer/allocate 4096)
         read-fn (:read fcol)]
     (try
       (let [n (.read sc buf)]
         (if (< n 0)
             (close-channel sk sc fcol)
             (do (.flip buf)
                 (if read-fn
                   (read-fn sc buf)))))
       (catch IOException e
         (close-channel sk sc fcol)))))

;;Bind a tcp server to localhost at port 8080,you can telnet it.
(def server
  (bind 
    (new InetSocketAddress 8080)
    {:read #(.write %1 %2)
     :created #(println "Accepted from" (.. % (socket) (getRemoteSocketAddress)))
     :closed  #(println "Disconnected from" (.. % (socket) (getRemoteSocketAddress)))
     }))

(defn- main 
	[ & args]
	(server))
