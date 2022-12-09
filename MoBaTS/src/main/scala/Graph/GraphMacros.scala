package Graph

import DSL.*

inline def modelToGraph[R, E](inline model: Model[R, E]): ModelGraph = ${ modelToGraphImpl[R, E]('{ model }) }
