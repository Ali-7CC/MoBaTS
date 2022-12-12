package DSL

inline def inspect(inline x: Any): String                        = ${ inspectCode('x) }
inline def modelPrinter[R, E](inline model: Model[R, E]): String = ${ modelPrinterImpl[R, E]('{ model }) }