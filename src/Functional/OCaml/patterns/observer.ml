type observer = string -> unit
type subscription = string * observer

let subscribe key observer subscribers =
  if List.exists (fun (existing_key, _) -> String.equal existing_key key) subscribers then
    (false, subscribers)
  else (true, (key, observer) :: subscribers)

let unsubscribe key subscribers =
  let remaining =
    List.filter (fun (existing_key, _) -> not (String.equal existing_key key)) subscribers
  in
  (List.length remaining <> List.length subscribers, remaining)

let publish event subscribers =
  List.iter (fun (_, notify) -> notify event) (List.rev subscribers)

let () =
  let audit_events = ref [] in
  let dashboard_events = ref [] in
  let audit event = audit_events := event :: !audit_events in
  let dashboard event = dashboard_events := event :: !dashboard_events in
  let added_audit, subscribers = subscribe "audit" audit [] in
  let added_dashboard, subscribers = subscribe "dashboard" dashboard subscribers in
  let duplicate_dashboard, subscribers = subscribe "dashboard" dashboard subscribers in
  assert added_audit;
  assert added_dashboard;
  assert (not duplicate_dashboard);
  publish "ready" subscribers;
  assert (!audit_events = [ "ready" ]);
  assert (!dashboard_events = [ "ready" ]);
  let removed_dashboard, subscribers = unsubscribe "dashboard" subscribers in
  let removed_dashboard_again, subscribers = unsubscribe "dashboard" subscribers in
  assert removed_dashboard;
  assert (not removed_dashboard_again);
  publish "done" subscribers;
  assert (!audit_events = [ "done"; "ready" ]);
  assert (!dashboard_events = [ "ready" ])
