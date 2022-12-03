(ns passel.http.client
  (:refer-clojure :exclude [send get])
  (:require
   [clojure.string :as str])
  (:import
   [java.time Duration]
   [java.net URI]
   [java.util.function Function]
   [java.net.http
    HttpClient
    HttpClient$Builder
    HttpClient$Redirect
    HttpClient$Version
    HttpRequest
    HttpRequest$BodyPublishers
    HttpRequest$Builder
    HttpResponse
    HttpResponse$BodyHandlers]
   [java.util.function Function Supplier]))

(set! *warn-on-reflection* true)

(defn convert-timeout [t]
  (if (integer? t)
    (Duration/ofMillis t)
    t))

(defmacro clj-fn->function ^Function [f]
  `(reify Function
    (~'apply [_# x#] (~f x#))))

(defn- version-keyword->version-enum [version]
  (case version
    :http1.1 HttpClient$Version/HTTP_1_1
    :http2   HttpClient$Version/HTTP_2))

(defn- convert-follow-redirect [redirect]
  (case redirect
    :always HttpClient$Redirect/ALWAYS
    :never  HttpClient$Redirect/NEVER
    :normal HttpClient$Redirect/NORMAL))

(defn client-builder
  (^HttpClient$Builder []
   (client-builder {}))
  (^HttpClient$Builder [opts]
   (let [{:keys [connect-timeout
                 cookie-handler
                 executor
                 follow-redirects
                 priority
                 proxy
                 ssl-context
                 ssl-parameters
                 version]} opts]
     (cond-> (HttpClient/newBuilder)
       connect-timeout  (.connectTimeout (convert-timeout connect-timeout))
       cookie-handler   (.cookieHandler cookie-handler)
       executor         (.executor executor)
       follow-redirects (.followRedirects (convert-follow-redirect follow-redirects))
       priority         (.priority priority)
       proxy            (.proxy proxy)
       ssl-context      (.sslContext ssl-context)
       ssl-parameters   (.sslParameters ssl-parameters)
       version          (.version (version-keyword->version-enum version))))))

(defn build-client
  (^HttpClient [] (.build (client-builder)))
  (^HttpClient [opts] (.build (client-builder opts))))

(def ^HttpClient default-client
  (delay (HttpClient/newHttpClient)))

(def ^:private byte-array-class
  (Class/forName "[B"))

(defn- input-stream-supplier [s]
  (reify Supplier
    (get [this] s)))

(defn- convert-body-publisher [body]
  (cond
    (nil? body)
    (HttpRequest$BodyPublishers/noBody)

    (string? body)
    (HttpRequest$BodyPublishers/ofString body)

    (instance? java.io.InputStream body)
    (HttpRequest$BodyPublishers/ofInputStream (input-stream-supplier body))

    (instance? byte-array-class body)
    (HttpRequest$BodyPublishers/ofByteArray body)))

(def ^:private convert-headers-xf
  (mapcat
    (fn [[k v :as p]]
      (if (sequential? v)
        (interleave (repeat k) v)
        p))))

(defn- method-keyword->str [method]
  (str/upper-case (name method)))

(defn request-builder ^HttpRequest$Builder [opts]
  (let [{:keys [expect-continue?
                headers
                method
                timeout
                uri
                version
                body]} opts]
    (cond-> (HttpRequest/newBuilder)
      (some? expect-continue?)
      (.expectContinue expect-continue?)

      (seq headers)
      (.headers (into-array String
                            (eduction convert-headers-xf headers)))

      method
      (.method (method-keyword->str method)
               (convert-body-publisher body))

      timeout
      (.timeout (convert-timeout timeout))

      uri
      (.uri (URI/create uri))

      version
      (.version (version-keyword->version-enum version)))))

(defn build-request
  (^HttpRequest [] (.build (request-builder {})))
  (^HttpRequest [req-map] (.build (request-builder req-map))))

(def ^:private bh-of-string (HttpResponse$BodyHandlers/ofString))
(def ^:private bh-of-input-stream (HttpResponse$BodyHandlers/ofInputStream))
(def ^:private bh-of-byte-array (HttpResponse$BodyHandlers/ofByteArray))

(defn- convert-body-handler [mode]
  (case mode
    nil           bh-of-string
    :string       bh-of-string
    :input-stream bh-of-input-stream
    :byte-array   bh-of-byte-array))

(defn- version-enum->version-keyword [^HttpClient$Version version]
  (case (.name version)
    "HTTP_1_1" :http1.1
    "HTTP_2"   :http2))

(defn response->map [^HttpResponse resp]
  {:status (.statusCode resp)
   :body (.body resp)
   :version (-> resp .version version-enum->version-keyword)
   :headers (into {}
                  (map (fn [[k v]] [k (if (> (count v) 1) (vec v) (first v))]))
                  (.map (.headers resp)))})


(def ^:private ^Function resp->ring-function
  (clj-fn->function response->map))

(defn- convert-request [req]
  (cond
    (map? req) (build-request req)
    (string? req) (build-request {:uri req})
    (instance? HttpRequest req) req))

(defn- send
  ([req]
   (send req {}))
  ([req {:keys [as client raw?] :as opts}]
   (let [^HttpClient client (or client @default-client)
         req' (convert-request req)
         resp (.send client req' (convert-body-handler as))]
     (if raw? resp (response->map resp)))))

(defn get
  ([uri]
   (send {:uri uri :headers {} :method :get}))
  ([uri headers]
   (send {:uri uri :headers headers :method :get}))
  ([uri headers opts]
   (send (merge {:uri uri :headers headers :method :get} opts))))

(defn post [uri headers body]
  (send {:uri uri :headers headers :body body :method :post}))

(comment
  (get "https://google.com"))
