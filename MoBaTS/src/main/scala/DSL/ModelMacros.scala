package DSL

inline def booleanToString(inline x: Any): String                        = ${ booleanToStringImpl('x) }
inline def modelPrinter[R, E](inline model: Model[R, E]): String = ${ modelPrinterImpl[R, E]('{ model }) }