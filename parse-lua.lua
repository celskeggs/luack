-- Based on https://github.com/stravant/LuaMinify/blob/master/ParseLua.lua
--
-- parse-lua.lua: The main lua parser and lexer.
--
-- returns an AST, internally relying on lex-lua.lua.

require 'strict'

local util = require 'util'
local Scope = require 'scope'
local lex = require 'lex-lua'
local build = require 'build'

return function(src)
	local st, tok
	if type(src) ~= 'table' then
		st, tok = lex(src)
	else
		st, tok = true, src
	end
	if not st then
		return false, tok
	end

	local function GenerateError(msg)
		local err = ">> :"..tok:Peek().Line..":"..tok:Peek().Char..": "..msg.."\n"
		--find the line
		local lineNum = 0
		if type(src) == 'string' then
			for line in src:gmatch("[^\n]*\n?") do
				if line:sub(-1,-1) == '\n' then line = line:sub(1,-2) end
				lineNum = lineNum+1
				if lineNum == tok:Peek().Line then
					err = err..">> `"..line:gsub('\t','    ').."`\n"
					for i = 1, tok:Peek().Char do
						local c = line:sub(i,i)
						if c == '\t' then
							err = err..'    '
						else
							err = err..' '
						end
					end
					err = err.."   ^^^^"
					break
				end
			end
		end
		return err
	end

	local ParseExpr, ParseStatementList, ParseSimpleExpr, ParseSubExpr, ParsePrimaryExpr, ParseSuffixedExpr

	local function ParseFunctionArgsAndBody(scope, tokenList, isLocal)
		local funcScope = Scope:new(scope)
		if not tok:ConsumeSymbol('(', tokenList) then
			return false, GenerateError("`(` expected.")
		end

		--arg list
		local argList = {}
		local isVarArg = false
		while not tok:ConsumeSymbol(')', tokenList) do
			if tok:Is('Ident') then
				local arg = funcScope:CreateLocal(tok:Get(tokenList).Data)
				argList[#argList+1] = arg
				if not tok:ConsumeSymbol(',', tokenList) then
					if tok:ConsumeSymbol(')', tokenList) then
						break
					else
						return false, GenerateError("`)` expected.")
					end
				end
			elseif tok:ConsumeSymbol('...', tokenList) then
				isVarArg = true
				if not tok:ConsumeSymbol(')', tokenList) then
					return false, GenerateError("`...` must be the last argument of a function.")
				end
				break
			else
				return false, GenerateError("Argument name or `...` expected")
			end
		end

		--body
		local st, body = ParseStatementList(funcScope)
		if not st then return false, body end

		--end
		if not tok:ConsumeKeyword('end', tokenList) then
			return false, GenerateError("`end` expected after function body")
		end
    
    return true, build.define(funcScope, argList, body, isVarArg, isLocal)
	end

	function ParsePrimaryExpr(scope)
		local tokenList = {}

		if tok:ConsumeSymbol('(', tokenList) then
			local st, ex = ParseExpr(scope)
			if not st then return false, ex end
			if not tok:ConsumeSymbol(')', tokenList) then
				return false, GenerateError("`)` Expected.")
			end
      return true, build.parentheses(ex)
		elseif tok:Is('Ident') then
			local id = tok:Get(tokenList)
			local var = scope:GetLocal(id.Data) or scope:global(id.Data)
      return true, build.lookup(var)
		else
			return false, GenerateError("primary expression expected")
		end
	end

	function ParseSuffixedExpr(scope, onlyDotColon)
		--base primary expression
		local st, prim = ParsePrimaryExpr(scope)
		if not st then return false, prim end
		--
		while true do
			local tokenList = {}

			if tok:IsSymbol('.') or tok:IsSymbol(':') then
				local symb = tok:Get(tokenList).Data
				if not tok:Is('Ident') then
					return false, GenerateError("<Ident> expected.")
				end
				local id = tok:Get(tokenList).Data
        prim = build.member(prim, symb == ":", id)
			elseif not onlyDotColon and tok:ConsumeSymbol('[', tokenList) then
				local st, ex = ParseExpr(scope)
				if not st then return false, ex end
				if not tok:ConsumeSymbol(']', tokenList) then
					return false, GenerateError("`]` expected.")
				end
        prim = build.index(prim, ex)
			elseif not onlyDotColon and tok:ConsumeSymbol('(', tokenList) then
				local args = {}
				while not tok:ConsumeSymbol(')', tokenList) do
					local st, ex = ParseExpr(scope)
					if not st then return false, ex end
					args[#args+1] = ex
					if not tok:ConsumeSymbol(',', tokenList) then
						if tok:ConsumeSymbol(')', tokenList) then
							break
						else
							return false, GenerateError("`)` Expected.")
						end
					end
				end
        prim = build.call(prim, args)
			elseif not onlyDotColon and tok:Is('String') then
        prim = build.stringcall(prim, tok:Get(tokenList))
			elseif not onlyDotColon and tok:IsSymbol('{') then
				--table call
				local st, ex = ParseSimpleExpr(scope)
				-- FIX: ParseExpr(scope) parses the table AND and any following binary expressions.
				-- We just want the table
				if not st then return false, ex end
        prim = build.tablecall(prim, ex)
			else
				break
			end
		end
		return true, prim
	end

	function ParseSimpleExpr(scope)
		local tokenList = {}

		if tok:Is('Number') then
      return true, build.number(tok:Get(tokenList))
		elseif tok:Is('String') then
      return true, build.string(tok:Get(tokenList))
		elseif tok:ConsumeKeyword('nil', tokenList) then
      return true, build.nilval()
		elseif tok:IsKeyword('false') or tok:IsKeyword('true') then
      return true, build.boolean(tok:Get(tokenList).Data == 'true')
		elseif tok:ConsumeSymbol('...', tokenList) then
      return true, build.dots(scope)
		elseif tok:ConsumeSymbol('{', tokenList) then
      local entries, nextid = {}, 1
			while true do
				if tok:IsSymbol('[', tokenList) then
					--key
					tok:Get(tokenList)
					local st, key = ParseExpr(scope)
					if not st then
						return false, GenerateError("Key Expression Expected")
					end
					if not tok:ConsumeSymbol(']', tokenList) then
						return false, GenerateError("`]` Expected")
					end
					if not tok:ConsumeSymbol('=', tokenList) then
						return false, GenerateError("`=` Expected")
					end
					local st, value = ParseExpr(scope)
					if not st then
						return false, GenerateError("Value Expression Expected")
					end
          table.insert(entries, {key, value})
				elseif tok:Is('Ident') then
					--value or key
					local lookahead = tok:Peek(1)
					if lookahead.Type == 'Symbol' and lookahead.Data == '=' then
						--we are a key
						local key = tok:Get(tokenList)
						if not tok:ConsumeSymbol('=', tokenList) then
							return false, GenerateError("`=` Expected")
						end
						local st, value = ParseExpr(scope)
						if not st then
							return false, GenerateError("Value Expression Expected")
						end
            table.insert(entries, {build.string(key.Data), value})
					else
						--we are a value
						local st, value = ParseExpr(scope)
						if not st then
							return false, GenerateError("Value Exected")
						end
            table.insert(entries, {build.number(nextid), value})
            nextid = nextid + 1
					end
				elseif tok:ConsumeSymbol('}', tokenList) then
					break
				else
					--value
					local st, value = ParseExpr(scope)
          table.insert(entries, {build.number(nextid), value})
          nextid = nextid + 1
					if not st then
						return false, GenerateError("Value Expected")
					end
				end

				if tok:ConsumeSymbol(';', tokenList) or tok:ConsumeSymbol(',', tokenList) then
					--all is good
				elseif tok:ConsumeSymbol('}', tokenList) then
					break
				else
					return false, GenerateError("`}` or table entry Expected")
				end
			end
			return true, build.table(entries)
		elseif tok:ConsumeKeyword('function', tokenList) then
			return ParseFunctionArgsAndBody(scope, tokenList, true)
		else
			return ParseSuffixedExpr(scope)
		end
	end

	local unops = util.lookupify{'-', 'not', '#'}
	local unopprio = 8
	local priority = {
		['+'] = {6,6};
		['-'] = {6,6};
		['%'] = {7,7};
		['/'] = {7,7};
		['*'] = {7,7};
		['^'] = {10,9};
		['..'] = {5,4};
		['=='] = {3,3};
		['<'] = {3,3};
		['<='] = {3,3};
		['~='] = {3,3};
		['>'] = {3,3};
		['>='] = {3,3};
		['and'] = {2,2};
		['or'] = {1,1};
	}
	function ParseSubExpr(scope, level)
		--base item, possibly with unop prefix
		local st, exp
		if unops[tok:Peek().Data] then
			local tokenList = {}
			local op = tok:Get(tokenList).Data
			st, exp = ParseSubExpr(scope, unopprio)
			if not st then return false, exp end
      exp = build.unop(op, exp)
		else
			st, exp = ParseSimpleExpr(scope)
			if not st then return false, exp end
		end

		--next items in chain
		while true do
			local prio = priority[tok:Peek().Data]
			if prio and prio[1] > level then
				local tokenList = {}
				local op = tok:Get(tokenList).Data
				local st, rhs = ParseSubExpr(scope, prio[2])
				if not st then return false, rhs end
        exp = build.binop(exp, op, rhs)
			else
        return true, exp
			end
		end
	end

	ParseExpr = function(scope)
		return ParseSubExpr(scope, 0)
	end

	local function ParseStatement(scope)
		local stat = nil
		local tokenList = {}
		if tok:ConsumeKeyword('if', tokenList) then
			--setup
      local clauses = {}
			--clauses
			repeat
				local st, nodeCond = ParseExpr(scope)
				if not st then return false, nodeCond end
				if not tok:ConsumeKeyword('then', tokenList) then
					return false, GenerateError("`then` expected.")
				end
				local st, nodeBody = ParseStatementList(scope)
				if not st then return false, nodeBody end
        table.insert(clauses, {nodeCond, nodeBody})
			until not tok:ConsumeKeyword('elseif', tokenList)
			--else clause
			if tok:ConsumeKeyword('else', tokenList) then
				local st, nodeBody = ParseStatementList(scope)
				if not st then return false, nodeBody end
        table.insert(clauses, {build.boolean(true), nodeBody})
			end
			--end
			if not tok:ConsumeKeyword('end', tokenList) then
				return false, GenerateError("`end` expected.")
			end
			stat = build.ifseq(clauses)
		elseif tok:ConsumeKeyword('while', tokenList) then
			--condition
			local st, nodeCond = ParseExpr(scope)
			if not st then return false, nodeCond end
			--do
			if not tok:ConsumeKeyword('do', tokenList) then
				return false, GenerateError("`do` expected.")
			end
			--body
			local st, nodeBody = ParseStatementList(scope)
			if not st then return false, nodeBody end
			--end
			if not tok:ConsumeKeyword('end', tokenList) then
				return false, GenerateError("`end` expected.")
			end
			stat = build.whiledo(nodeCond, nodeBody)
		elseif tok:ConsumeKeyword('do', tokenList) then
			--do block
			local st, nodeBlock = ParseStatementList(scope)
			if not st then return false, nodeBlock end
			if not tok:ConsumeKeyword('end', tokenList) then
				return false, GenerateError("`end` expected.")
			end
      stat = build.doblock(nodeBlock)
		elseif tok:ConsumeKeyword('for', tokenList) then
			--for block
			if not tok:Is('Ident') then
				return false, GenerateError("<ident> expected.")
			end
			local baseVarName = tok:Get(tokenList)
			if tok:ConsumeSymbol('=', tokenList) then
				--numeric for
				local forScope = Scope:new(scope)
				local forVar = forScope:CreateLocal(baseVarName.Data)
				--
				local st, startEx = ParseExpr(scope)
				if not st then return false, startEx end
				if not tok:ConsumeSymbol(',', tokenList) then
					return false, GenerateError("`,` Expected")
				end
				local st, endEx = ParseExpr(scope)
				if not st then return false, endEx end
				local st, stepEx;
				if tok:ConsumeSymbol(',', tokenList) then
					st, stepEx = ParseExpr(scope)
					if not st then return false, stepEx end
				end
				if not tok:ConsumeKeyword('do', tokenList) then
					return false, GenerateError("`do` expected")
				end
				--
				local st, body = ParseStatementList(forScope)
				if not st then return false, body end
				if not tok:ConsumeKeyword('end', tokenList) then
					return false, GenerateError("`end` expected")
				end
        stat = build.numericfor(forScope, forVar, startEx, endEx, stepEx, body)
			else
				--generic for
				local forScope = Scope:new(scope)
				local varList = { forScope:CreateLocal(baseVarName.Data) }
				while tok:ConsumeSymbol(',', tokenList) do
					if not tok:Is('Ident') then
						return false, GenerateError("for variable expected.")
					end
          table.insert(varList, forScope:CreateLocal(tok:Get(tokenList).Data))
				end
				if not tok:ConsumeKeyword('in', tokenList) then
					return false, GenerateError("`in` expected.")
				end
				local st, firstGenerator = ParseExpr(scope)
				if not st then return false, firstGenerator end
				local generators = { firstGenerator }
				while tok:ConsumeSymbol(',', tokenList) do
					local st, gen = ParseExpr(scope)
					if not st then return false, gen end
          table.insert(generators, gen)
				end
				if not tok:ConsumeKeyword('do', tokenList) then
					return false, GenerateError("`do` expected.")
				end
				local st, body = ParseStatementList(forScope)
				if not st then return false, body end
				if not tok:ConsumeKeyword('end', tokenList) then
					return false, GenerateError("`end` expected.")
				end
        stat = build.genericfor(forScope, varList, generators, body)
			end
		elseif tok:ConsumeKeyword('repeat', tokenList) then
			local st, body, subscope = ParseStatementList(scope)
			if not st then return false, body end
			if not tok:ConsumeKeyword('until', tokenList) then
				return false, GenerateError("`until` expected.")
			end
			local st, cond = ParseExpr(subscope)
			if not st then return false, cond end
      stat = build.repeatuntil(cond, body)
		elseif tok:ConsumeKeyword('function', tokenList) then
			if not tok:Is('Ident') then
				return false, GenerateError("Function name expected")
			end
			local st, name = ParseSuffixedExpr(scope, true) --true => only dots and colons
			if not st then return false, name end
			local st, func = ParseFunctionArgsAndBody(scope, tokenList, false)
			if not st then return false, func end
      stat = build.store(name, func)
		elseif tok:ConsumeKeyword('local', tokenList) then
			if tok:Is('Ident') then
				local varList = { tok:Get(tokenList).Data }
				while tok:ConsumeSymbol(',', tokenList) do
					if not tok:Is('Ident') then
						return false, GenerateError("local var name expected")
					end
          table.insert(varList, tok:Get(tokenList).Data)
				end

				local initList = {}
				if tok:ConsumeSymbol('=', tokenList) then
					repeat
						local st, ex = ParseExpr(scope)
						if not st then return false, ex end
            table.insert(initList, ex)
					until not tok:ConsumeSymbol(',', tokenList)
				end

				--now patch var list
				--we can't do this before getting the init list, because the init list does not
				--have the locals themselves in scope.
				for i, v in pairs(varList) do
					varList[i] = scope:CreateLocal(v)
				end

        stat = build.locals(varList, initList)
			elseif tok:ConsumeKeyword('function', tokenList) then
				if not tok:Is('Ident') then
					return false, GenerateError("Function name expected")
				end
				local name = tok:Get(tokenList).Data
				local localVar = scope:CreateLocal(name)
				--
				local st, func = ParseFunctionArgsAndBody(scope, tokenList, true)
				if not st then return false, func end
				stat = build.locals({ localVar }, { func })
			else
				return false, GenerateError("local var or function def expected")
			end
		elseif tok:ConsumeSymbol('::', tokenList) then
			if not tok:Is('Ident') then
				return false, GenerateError('Label name expected')
			end
			local label = tok:Get(tokenList).Data
			if not tok:ConsumeSymbol('::', tokenList) then
				return false, GenerateError("`::` expected")
			end
      stat = build.label(label)
		elseif tok:ConsumeKeyword('return', tokenList) then
			local exList = {}
			if not tok:IsKeyword('end') then
				local st, firstEx = ParseExpr(scope)
				if st then
					exList[1] = firstEx
					while tok:ConsumeSymbol(',', tokenList) do
						local st, ex = ParseExpr(scope)
						if not st then return false, ex end
            table.insert(exList, ex)
					end
				end
			end
      stat = build.returns(exList)
		elseif tok:ConsumeKeyword('break', tokenList) then
      stat = build.dobreak()
		elseif tok:ConsumeKeyword('goto', tokenList) then
			if not tok:Is('Ident') then
				return false, GenerateError("Label expected")
			end
			local label = tok:Get(tokenList).Data
      stat = build.dogoto(label)
		else
			--statementParseExpr
			local st, suffixed = ParseSuffixedExpr(scope)
			if not st then return false, suffixed end

			--assignment or call?
			if tok:IsSymbol(',') or tok:IsSymbol('=') then
				--TODO: check that it was not parenthesized, making it not an lvalue
				--if not build.islvalue(suffixed) then
				--	return false, GenerateError("Can not assign to parenthesized expression, is not an lvalue")
				--end

				--more processing needed
				local lhs = { suffixed }
				while tok:ConsumeSymbol(',', tokenList) do
					local st, lhsPart = ParseSuffixedExpr(scope)
					if not st then return false, lhsPart end
          table.insert(lhs, lhsPart)
				end
				--equals
				if not tok:ConsumeSymbol('=', tokenList) then
					return false, GenerateError("`=` Expected.")
				end
				--rhs
				local st, firstRhs = ParseExpr(scope)
				if not st then return false, firstRhs end
				local rhs = { firstRhs }
				while tok:ConsumeSymbol(',', tokenList) do
					local st, rhsPart = ParseExpr(scope)
					if not st then return false, rhsPart end
          table.insert(rhs, rhsPart)
				end
        stat = build.store_many(lhs, rhs)
			else
        stat = build.expr_to_stmt(suffixed)
        if not stat then
          return false, GenerateError("Assignment Statement Expected")
        end
      end
		end

		if tok:IsSymbol(';') then
			tok:Get()
		end
		return true, stat
	end

	local statListCloseKeywords = util.lookupify{'end', 'else', 'elseif', 'until'}

	ParseStatementList = function(scope, endwitheof)
		local newscope, body = Scope:new(scope), {}
    
		while not statListCloseKeywords[tok:Peek().Data] and not tok:IsEof() do
			local st, nodeStatement = ParseStatement(newscope)
      print("Finished ParseStatement:", st, nodeStatement and nodeStatement[1])
			if not st then return false, nodeStatement end
      table.insert(body, nodeStatement)
		end

    if endwitheof then
      if not tok:IsEof() then
        return false, GenerateError("EOF expected")
      else
        return true, build.block(newscope, body)
      end
    else
      if tok:IsEof() then
        return false, GenerateError("EOF unexpected")
      else
        return true, build.block(newscope, body), newscope
      end
		end
	end

  local topScope = Scope:new()
  topScope:CreateLocal("_ENV")
  return ParseStatementList(topScope, true)
end
