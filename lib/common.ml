
open Format

let print_trace () =
  let trace = Printexc.raw_backtrace_to_string
      (Printexc.get_callstack 20) in
  printf "%s" trace

let todo loc = 
  print_trace ();
  printf "%s: todo\n" loc;
  exit 1

let impossible loc = 
  print_trace ();
  printf "%s: impossible\n" loc;
  exit 1

