exception CpuTimeLimitExceeded
exception MemoryLimitExceeded

let cpu_time_limit = ref infinity
let memory_limit = ref max_int (* memory limit stored in words *)

let check_cpu_time_limit () =
  if Sys.time () >= !cpu_time_limit then
    raise CpuTimeLimitExceeded

let check_memory_limit () =
  if (Gc.quick_stat ()).Gc.heap_words >= !memory_limit then
    raise MemoryLimitExceeded
