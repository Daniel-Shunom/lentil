import dev/stress_test/build_session.{build_session}
import dev/stress_test/build_chatsession.{build_chatsession}
import dev/stress_test/dev_consts
import gleam/result

pub fn init_devtest(first: String, last: String) {
  use session <- result.try(build_session(first, last))
  let assert Ok(subj) = build_chatsession(session)
  todo
}

