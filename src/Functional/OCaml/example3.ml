module type Report = sig
  val generate : unit -> unit
end

module PDFReport : Report = struct
  let generate () = print_endline "Generating PDF report"
end

module HTMLReport : Report = struct
  let generate () = print_endline "Generating HTML report"
end

module type ReportFactory = sig
  val create_report : unit -> (module Report)
end

module PDFReportFactory : ReportFactory = struct
  let create_report () = (module PDFReport : Report)
end

module HTMLReportFactory : ReportFactory = struct
  let create_report () = (module HTMLReport : Report)
end

let use_factory (factory : (module ReportFactory)) =
  let module F = (val factory : ReportFactory) in
  let module R = (val F.create_report () : Report) in
  R.generate ()

(* Usage *)
let () =
  use_factory (module PDFReportFactory);
  use_factory (module HTMLReportFactory)
