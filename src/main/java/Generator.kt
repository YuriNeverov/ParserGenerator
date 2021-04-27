import java.io.File
import java.io.FileWriter
import java.lang.RuntimeException

class Rule(val left: String,
           val fields: String,
           val args: String,
           val terms: MutableList<String> = mutableListOf(),
           val termArgs: MutableList<String> = mutableListOf(),
           val codeBetween: MutableList<String> = mutableListOf())

class Generator(private val packageName: String, private val grammar: String) {
    private val srcRoot: File = File("_src").apply { mkdirs() }
    private val javaRoot: File = srcRoot.resolve("java").apply { mkdirs() }
    private val packageRoot: File = javaRoot.resolve(packageName).apply { mkdirs() }
    private val tokenPath = packageRoot.resolve("token").apply { mkdirs() }
    private val tokenFilename = "ValueToken"
    private val lexerPath = packageRoot.resolve("lexer").apply { mkdirs() }
    private val lexerFilename = "Lexer"
    private val treePath = packageRoot.resolve("tree").apply { mkdirs() }
    private val treeFilename = "Node"
    private val parserFilename = "Parser"

    private val lexerPattern = """([A-Z][a-zA-Z0-9]*?)(~)?(\((.*?)\))?: (.*?);\s*?\n"""
    private val rulePattern = """([a-z][a-zA-Z0-9]*?)(\[(.*?)])?(\((.*?)\))?\s*?->\s*(.*?);\s*?\n"""
    private val startTermConfigPattern = """@Start --> ([a-z][a-zA-Z0-9]*);"""
    private val codePrefPattern = """\{\s*(.*?)\s*}\."""
    private val rightPattern = """([a-zA-Z0-9]+)\s*(\((.*?)\))?\s*(\{\s*(.*?)\s*}\.)?"""

    private val tokens = mutableListOf("EPS")
    private val tokenToNumber = mutableMapOf("EPS" to 0)
    private val rules = mutableListOf<Rule>()
    private val termToRules = mutableMapOf<String, MutableList<Int>>()
    private val first = mutableMapOf<String, MutableSet<String>>()
    private val follow = mutableMapOf<String, MutableSet<String>>()
    private val term2rule2tokens = mutableMapOf<String, MutableMap<Int, MutableSet<String>>>()

    private lateinit var startTerm: String

    // Meta
    private val parseExceptionWindowSize = 25

    fun generateLexerTokens() {
        val lexerRegex = Regex(lexerPattern)
        var last = 0
        val tokenClasses = StringBuilder(
            "package $packageName.token\n" +
            "open class $tokenFilename(val text: String)\n" +
            "class EOFToken() : $tokenFilename(\"$\")\n")
        val patterns = StringBuilder("listOf<Regex>(")
        val nextTokenBranches = StringBuilder()

        var find = lexerRegex.find(grammar, last)
        var i = 1
        while (find != null) {
            last = find.range.last + 1
            val destruct = find.destructured
            var (name, skip, _, attrs, regex) = destruct
            if (attrs.isNotBlank()) attrs = ", $attrs"
            tokens.add(name)
            tokenToNumber.putIfAbsent(name, i)

            if (skip.isBlank())
                tokenClasses.append("class ${name}Token(text: String$attrs) : $tokenFilename(text)\n")

            patterns.append((if (i == 1) "" else ", ") + "Regex(\"\"\"^$regex\"\"\")")

            if (i > 1) nextTokenBranches.append('\n')
            nextTokenBranches.append(
                "\t\tval r${i-1} = patterns[${i-1}].find(text)\n" +
                "\t\tif (r${i-1} != null) {\n" +
                "\t\t\ttext = text.substring(r${i-1}.range.last + 1)\n",
                "\t\t\tcurPos = curPos + r${i-1}.range.last + 1\n")
            if (skip.isBlank())
                nextTokenBranches.append("\t\t\tthis.token = ${name}Token(r${i-1}.value)\n\t\t\treturn\n")
            else
                nextTokenBranches.append("\t\t\tnextToken()\n\t\t\treturn\n")
            nextTokenBranches.append("\t\t}\n")
            find = lexerRegex.find(grammar, last)
            i++
        }
        patterns.append(')')

        val tokenFile = tokenPath.resolve("$tokenFilename.kt").apply { createNewFile() }
        FileWriter(tokenFile).use {
            it.write(tokenClasses.toString())
        }

        val lexerFile = lexerPath.resolve("$lexerFilename.kt").apply { createNewFile() }
        FileWriter(lexerFile).use {
            it.write("package $packageName.lexer\n" +
                    "import java.io.InputStream\n" +
                    "import $packageName.token.*\n\n")
            it.append("class $lexerFilename(input: InputStream?) {\n")
            it.append("\tvar text = input?.readAllBytes()?.decodeToString() ?: \"\"\n")
            it.append("\tval patterns = $patterns\n")
            it.append("\tvar curPos = 0\n")
            it.append("\tvar token: $tokenFilename? = null\n\n")
            it.append("\tconstructor(s: String) : this(null) {\n")
            it.append("\t\ttext = s\n")
            it.append("\t}\n\n")
            it.append("\tfun nextToken() {\n")
            it.append(nextTokenBranches.toString())
            it.append("\t\tif (text.isBlank()) this.token = EOFToken() else throw RuntimeException(\"No more tokens recognised: \$text\")\n")
            it.append("\t}\n}\n")
        }

    }

    fun getRules() {
        val ruleRegex = Regex(rulePattern)
        var last = 0
        var find = ruleRegex.find(grammar, last)
        val start = Regex(startTermConfigPattern).find(grammar) ?: throw RuntimeException("No start non-terminal found") // TODO: Custom exceptions
        startTerm = start.destructured.component1()
        println("Start non-terminal found: '$startTerm'")
        var i = 0
        while (find != null) {
            last = find.range.last + 1
            val (left, _, fields, _, args, rightPart) = find.destructured
            val rule = Rule(left, fields, args)
            val codePrefRegex = Regex(codePrefPattern)
            val rightRegex = Regex(rightPattern)
            var termLast = 0
            var codePref = ""
            if (rightPart[0] == '{') {
                val parsed = codePrefRegex.find(rightPart)!!
                codePref = parsed.destructured.component1()
                termLast = parsed.range.last + 1
            }
            rule.codeBetween.add(codePref)
            var parsed = rightRegex.find(rightPart, termLast)
            while (parsed != null) {
                termLast = parsed.range.last + 1
                val (rightName, _, termArgs, _, termAfterCode) = parsed.destructured
                rule.terms.add(rightName)
                rule.termArgs.add(termArgs)
                rule.codeBetween.add(termAfterCode)
                parsed = rightRegex.find(rightPart, termLast)
            }
            if (left in termToRules.keys) {
                termToRules[left]?.add(i)
            } else {
                termToRules[left] = mutableListOf(i)
            }
            rules.add(rule)
            i++
            find = ruleRegex.find(grammar, last)
        }
        var j = 0
        for (rule in rules) {
            print("$j: ${rule.left} [${rule.fields}] (${rule.args}) -> {${rule.codeBetween[0]}}")
            for (i in 0 until rule.terms.size) {
                print(" |${rule.terms[i]}|(${rule.termArgs[i]}) {${rule.codeBetween[i + 1]}}")
            }
            println()
            j++
        }
        for (term in termToRules.keys) {
            println("$term: ${termToRules[term]!!.joinToString { it.toString() }}")
        }

    }

    fun FIRST(alpha: List<String>, start: Int = 0) : MutableSet<String> {
        val res = mutableSetOf<String>()
        for (i in start .. alpha.size) {
            if (i == alpha.size) {
                res.add("EPS")
                break
            }
            val a = alpha[i]
            if (a in tokens) {
                res.add(a)
                break
            } else if (a.isBlank()) {
                res.add("EPS")
                break
            } else {
                if (a in first.keys) {
                    val fst = first[a]!!
                    if ("EPS" in fst) {
                        for (obj in fst) {
                            if (obj != "EPS") res.add(obj)
                        }
                    } else {
                        res.addAll(fst)
                        break
                    }
                } else {
                    throw RuntimeException("Error occurred while building FIRST. Unknown term: '$a' in rule chain '${alpha.joinToString { it }}'") // TODO: Custom exceptions
                }
            }
        }
        return res
    }

    fun calculateFirstFollow() {
        // Calculating FIRST
        first["EPS"] = mutableSetOf("EPS")
        for (token in tokens) {
            first[token] = mutableSetOf(token)
        }
        for (term in termToRules.keys) {
            first[term] = mutableSetOf()
        }
        var changed = true
        while (changed) {
            changed = false
            for (rule in rules) {
                val set = FIRST(rule.terms)
                for (token in set) {
                    val fst = first[rule.left]!!
                    if (token !in fst) {
                        fst.add(token)
                        changed = true
                    }
                }
            }
        }

        // Calculating FOLLOW
        for (term in termToRules.keys) {
            follow[term] = mutableSetOf()
        }
        follow[startTerm]!!.add("EOF")
        changed = true
        while (changed) {
            changed = false
            for (rule in rules) {
                val flwA = follow[rule.left]!!
                for (i in rule.terms.indices) {
                    val term = rule.terms[i]
                    if (term in termToRules.keys) {
                        val flwB = follow[term]!!
                        val fst = FIRST(rule.terms, i + 1)
                        if ("EPS" in fst) {
                            for (token in fst) {
                                if (token != "EPS" && token !in flwB)  {
                                    flwB.add(token)
                                    changed = true
                                }
                            }
                            for (token in flwA) {
                                if (token !in flwB) {
                                    flwB.add(token)
                                    changed = true
                                }
                            }
                        } else {
                            for (token in fst) {
                                if (token !in flwB)  {
                                    flwB.add(token)
                                    changed = true
                                }
                            }
                        }
                    }
                }
            }
        }

        // Structuring rules
        for (term in termToRules.keys) {
            term2rule2tokens.putIfAbsent(term, mutableMapOf())
            val rule2tokens = term2rule2tokens[term]!!
            for (ruleId in termToRules[term]!!) {
                rule2tokens.putIfAbsent(ruleId, mutableSetOf())
                val set = rule2tokens[ruleId]!!

                val rule = rules[ruleId]

                val fst = FIRST(rule.terms)
                if ("EPS" in fst) {
                    for (token in fst) {
                        if (token != "EPS") set.add(token)
                    }
                    set.addAll(follow[term]!!)
                } else {
                    set.addAll(fst)
                }
            }
        }

        println("FIRST:")
        for (a in first.keys) {
            val v = first[a]!!
            println("$a: ${v.joinToString { it }}")
        }
        println("FOLLOW:")
        for (a in follow.keys) {
            val v = follow[a]!!
            println("$a: ${v.joinToString { it }}")
        }
        println("\n")

        // Checking LL(1)
        for (term in termToRules.keys) {
            val rule2tokens = term2rule2tokens[term]!!
            val rIds = rule2tokens.keys.toList()
            println("$term -> $rIds")
            for (i in rIds.indices) {
                val a = rule2tokens[rIds[i]]!!
                for (j in i + 1 until rIds.size) {
                    val b = rule2tokens[rIds[j]]!!
                    println("{$a} and {$b}")
                    val intersection = a.intersect(b)
                    if (intersection.isNotEmpty()) {
                        throw RuntimeException("Grammar is not LL(1): for $term rule FIRST(${rIds[i]}) and FIRST(${rIds[j]}) are not disjunctive (contain $intersection)") // TODO: Custom exceptions
                    }
                }
            }

            for (i in rIds) {
                val a = rule2tokens[i]!!
                if ("EPS" in a) {
                    val flw = follow[term]!!
                    for (j in rIds) {
                        if (j == i) continue
                        val b = rule2tokens[j]!!
                        val intersection = flw.intersect(b)
                        if (intersection.isNotEmpty()) {
                            throw RuntimeException("Grammar is not LL(1): for $term rule while FIRST($i) contains EPS, FIRST($j) and FOLLOW($term) are not disjunctive (contain $intersection)") // TODO: Custom exceptions
                        }
                    }
                    break
                }
            }
        }
    }

    fun generateParser() {
        val treeFile = treePath.resolve("$treeFilename.kt").apply { createNewFile() }
        FileWriter(treeFile).use {
            it.write("package $packageName.tree\n" +
                    "import java.io.InputStream\n" +
                    "import $packageName.token.*\n\n")
            it.append("interface $treeFilename\n")
            it.append("class Terminal(val token: ValueToken) : $treeFilename\n")
            it.append("open class NonTerminal(val children: MutableList<$treeFilename> = mutableListOf()) : $treeFilename\n")
            for (term in termToRules.keys) {
                val someRule = rules[termToRules[term]!![0]]
                it.append("class ${term.capitalize()}(${someRule.fields}) : NonTerminal()\n")
            }
        }
        val parserFile = packageRoot.resolve("$parserFilename.kt").apply { createNewFile() }
        FileWriter(parserFile).use {
            it.write("package $packageName\n" +
                    "import java.lang.Integer.min\n" +
                    "import java.text.ParseException\n" +
                    "import java.lang.Math.pow\n" +
                    "import $packageName.token.*\n" +
                    "import $packageName.lexer.$lexerFilename\n" +
                    "import $packageName.tree.*\n\n")

            it.append("class $parserFilename(val lex: $lexerFilename) {\n")
            it.append("\tval window = $parseExceptionWindowSize\n")
            it.append("\tfun parse() : ${startTerm.capitalize()} {\n" +
                        "\t\tlex.nextToken()\n" +
                        "\t\treturn $startTerm()\n" +
                      "\t}\n\n")

            it.append("\tfun expect(clazz: Class<out ValueToken>) {\n" +
                    "\t\tif (lex.token?.javaClass != clazz) {\n" +
                    "\t\t\tthrow ParseException(\"Expected \${clazz.simpleName}, got \${lex.token?.javaClass?.simpleName} at pos \${lex.curPos} near '\${lex.text.substring(0, min(window, lex.text.length))}'\", lex.curPos)\n" +
                    "\t\t}\n" +
                    "\t}\n\n")

            for (term in termToRules.keys) {
                val ruls = termToRules[term]!!
                val rule2tokens = term2rule2tokens[term]!!
                val firstRule = rules[ruls[0]]
                it.append("\tfun $term(${firstRule.args}) : ${term.capitalize()} {\n")
                it.append("\t\tval that = ${term.capitalize()}()\n")
                it.append("\t\twhen (lex.token!!) {\n")
                for (ruleId in ruls) {
                    val rule = rules[ruleId]
                    val tokenz: MutableSet<String> = rule2tokens[ruleId]!!
                    it.append("\t\t\t${tokenz.joinToString { tok -> "is ${tok}Token" }} -> {\n")
                    val pref = rule.codeBetween[0]
                    if (pref.isNotBlank()) {
                        it.append("\t\t\t\t$pref\n")
                    }
                    for (i in 0 until rule.terms.size) {
                        val curTerm = rule.terms[i]
                        val curTermArgs = rule.termArgs[i]
                        val curCodeAfter = rule.codeBetween[i + 1]
                        if (curTerm in tokens) {
                            if (curTerm != "EPS") {
                                it.append("\t\t\t\texpect(${curTerm}Token::class.java)\n")
                                it.append("\t\t\t\tval v$i = Terminal(lex.token!!)\n")
                                it.append("\t\t\t\tthat.children.add(v$i)\n")
                                it.append("\t\t\t\tlex.nextToken()\n")

                            }
                        } else {
                            it.append("\t\t\t\tval v$i = $curTerm($curTermArgs)\n")
                            it.append("\t\t\t\tthat.children.add(v$i)\n")
                        }
                        if (curCodeAfter.isNotBlank()) {
                            it.append("\t\t\t\t$curCodeAfter\n")
                        }
                    }
                    it.append("\t\t\t}\n")
                }
                it.append("\t\t\telse -> throw ParseException(\"Unexpected token: \${lex.token} at pos \${lex.curPos} near '\${lex.text.substring(0, min(window, lex.text.length))}'\", lex.curPos)\n")
                it.append("\t\t}\n\t\treturn that\n\t}\n")
            }
            it.append("}\n")
        }
    }
}