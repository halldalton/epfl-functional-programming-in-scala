val f: PartialFunction[String, String] = {case "ping" => "pong"}

f.isDefinedAt("ping")
f.isDefinedAt("abc")