(ns example3)

(defprotocol ReportFactory
  (generate [this]))

(defrecord PDFReportFactory []
  ReportFactory
  (generate [_] (println "Generating PDF report")))

(defrecord HTMLReportFactory []
  ReportFactory
  (generate [_] (println "Generating HTML report")))

(defn use-factory [factory]
  (generate factory))

;; Usage
(use-factory (->PDFReportFactory))
(use-factory (->HTMLReportFactory))
