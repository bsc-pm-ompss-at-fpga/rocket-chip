/*
 * Questions:
 *
 * 1. What is the basic meaning of each of the four interfaces?
 *    - in_req: Vector interface for receiving data requests
 *    - in_resp: Vector interface that should receive data as prescribed by this arbiter.
 *    - out_req: ?
 *    - out_resp: Data interface whose data is consumed and directed to adequate receivers by this arbiter.
 *
 * 2. Under what conditions will this arbiter avoid consuming requests?
 */


/** This arbiter preserves the order of responses */
class InOrderArbiter[T <: Data, U <: Data](reqTyp: T, respTyp: U, n: Int)
    (implicit p: Parameters) extends Module {
  val io = new Bundle {
    val in_req = Vec(n, Decoupled(reqTyp)).flip
    val in_resp = Vec(n, Decoupled(respTyp))
    val out_req = Decoupled(reqTyp)
    val out_resp = Decoupled(respTyp).flip
  }

  if (n > 1) {
    val route_q = Module(new Queue(UInt(width = log2Up(n)), 2))
    val req_arb = Module(new RRArbiter(reqTyp, n))
    req_arb.io.in <> io.in_req

    val req_helper = DecoupledHelper(
      req_arb.io.out.valid,
      route_q.io.enq.ready,
      io.out_req.ready)

    io.out_req.bits := req_arb.io.out.bits

    // The following is identical to req_arb.io.out.valid && route_q.io.enq.ready
    //
    // This is true iff the RRArbiter receiving data requests has a valid output and
    // the single queue that should store such requests is ready to receive them
    io.out_req.valid := req_helper.fire(io.out_req.ready)

    route_q.io.enq.bits := req_arb.io.chosen
    
    // The following is identical to req_arb.io.out.valid && io.out_req.ready
    //
    // This is true iff the RRArbiter receiving data requests has a valid output and
    // the out_req interface is ready
    route_q.io.enq.valid := req_helper.fire(route_q.io.enq.ready)

    // The following is identical to route_q.io.enq.ready && io.out_req.ready
    // 
    // This is true iff the queue storing data requests is ready and
    // the out_req interface is ready
    req_arb.io.out.ready := req_helper.fire(req_arb.io.out.valid)

    val resp_sel = route_q.io.deq.bits
    val resp_ready = io.in_resp(resp_sel).ready
    val resp_helper = DecoupledHelper(
      resp_ready,
      route_q.io.deq.valid,
      io.out_resp.valid)

    // The following is identical to route_q.io.deq.valid && io.out_resp.valid
    // 
    // This is true iff the queue storing data requests has a valid packet and
    // the central interface that should be consumed has valid data
    val resp_valid = resp_helper.fire(resp_ready)
    for (i <- 0 until n) {
      io.in_resp(i).bits := io.out_resp.bits
      io.in_resp(i).valid := resp_valid && resp_sel === UInt(i)
    }

    // The following is identical to io.in_resp(resp_sel).ready && io.out_resp.valid
    // 
    // This is true iff the vector interface element that is entitled to receive data this turn is ready and
    // the central interface that should be consumed has valid data
    route_q.io.deq.ready := resp_helper.fire(route_q.io.deq.valid)
    
    // The following is identical to io.in_resp(resp_sel).ready && route_q.io.deq.valid
    // 
    // This is true iff the vector interface element that is entitled to receive data this turn is ready and
    // the queue storing data requests has a valid output
    io.out_resp.ready := resp_helper.fire(io.out_resp.valid)
  } else {
    io.out_req <> io.in_req.head
    io.in_resp.head <> io.out_resp
  }
}
