
local util = require "util"

local build = {}

function build.define(funcScope, argList, body, isVarArg, isLocal)
  return {"define", funcScope, argList, body, isVarArg, isLocal}
end

function build.parentheses(ex)
  return ex
end

function build.getlocals()
  return {"locals"}
end

function build.getlocal(name, layer)
  if layer == 0 then
    return build.member(build.getlocals(), false, name)
  else
    assert(layer > 0)
    return build.member(build.getlocal("_PARENT", layer - 1), false, name)
  end
end

function build.lookup(var)
  if var.IsGlobal then
    return build.member(build.lookup(var.Scope:GetLocal("_ENV")), false, var.Name)
  else
    return build.getlocal(var.Name, var.Layer)
  end
end

function build.member(base, isColon, id)
  if isColon then
    return {"colon-member", base, id}
  else
    return build.index(base, build.string(id))
  end
end

function build.index(prim, ex)
  return {"index", prim, ex}
end

function build.call(prim, args)
  if prim[1] == "colon-member" then -- colon access
    return {"call-method", prim[2], prim[3], args} -- {"call-method", a, b, {...}) means a:b(...) which means a.b(a, ...)
  else
    return {"call", prim, args}
  end
end

function build.stringcall(prim, str)
  return build.call(prim, { build.string(str) })
end

function build.tablecall(prim, tableEx)
  return build.call(prim, { tableEx })
end

function build.number(value)
  return {"number", value}
end

function build.string(value)
  return {"string", value}
end

function build.nilval()
  return {"nil"}
end

function build.boolean(value)
  return {"boolean", value}
end

function build.dots(scope)
  return {"dots"}
end

function build.table(entries)
  table.insert(entries, 1, "table")
  return entries
end

function build.store(name, value) -- name is also an expression, but not to be evaluated
  if name[1] == "getvar" then
    return {"setvar", name[2], value}
  elseif name[1] == "index" then
    return {"setindex", name[2], name[3], value}
  elseif name[1] == "member" then
    assert(not name[3]) -- cannot have a colon
    return {"setmember", name[2], name[4], value}
  else
    error("cannot store into " .. name[1])
  end
end

function build.unop(op, exp)
  return {"unary", op, exp}
end

function build.binop(lhs, op, rhs)
  return {"binary", lhs, op, rhs}
end

function build.ifseq(clauses)
  table.insert(clauses, 1, "ifseq")
  return clauses
end

function build.whiledo(cond, body)
  return {"while", cond, body}
end

function build.doblock(block)
  return block
end

function build.numericfor(forScope, forVar, startEx, endEx, stepEx, body)
  return {"numeric-for", forScope, forVar, startEx, endEx, stepEx, body}
end

function build.genericfor(forScope, varList, generators, body)
  return {"generic-for", forScope, varList, generators, body}
end

function build.repeatuntil(cond, body)
  return {"repeat", cond, body}
end

function build.locals(varList, initList)
  -- return {"locals", varList, initList}
  for k, v in ipairs(varList) do
    varList[k] = build.lookup(v)
  end
  return build.store_many(varList, initList)
end

function build.label(label)
  return {"label", label}
end

function build.returns(exList)
  return {"return", exList}
end

function build.dobreak()
  return {"dobreak"}
end

function build.dogoto(label)
  return {"goto", label}
end

local no_side_effects = util.lookupify{"define", "getvar", "number", "string", "nil", "boolean", "dots"}
local function from_expr_to_side_effects(expr, blk)
  if not no_side_effects[expr[1]] then
    local as_stmt = build.expr_to_stmt(expr[1])
    if as_stmt then -- handles "call"
      table.insert(blk, as_stmt)
    elseif expr[1] == "member" then
      return from_expr_to_side_effects(expr[2], blk)
    elseif expr[1] == "index" then
      from_expr_to_side_effects(expr[3], blk)
      return from_expr_to_side_effects(expr[2], blk)
    elseif expr[1] == "unary" then -- NOTE: this means that any ~errors~ caused by an invalid unary operation... could be thrown away! TODO: is this okay?
      return from_expr_to_side_effects(expr[3], blk)
    elseif expr[1] == "binary" then -- NOTE: this means that any ~errors~ caused by an invalid binary operation... could be thrown away! TODO: is this okay?
      from_expr_to_side_effects(expr[2], blk)
      return from_expr_to_side_effects(expr[4], blk)
    elseif expr[1] == "table" then
      for i=2,#expr do
        local keyEx, valueEx = expr[i][1], expr[i][2]
        from_expr_to_side_effects(keyEx, blk)
        from_expr_to_side_effects(valueEx, blk)
      end
    else
      error("no implementation for extraction of side effects from expression: " .. expr[1])
    end
  end
end

function build.store_many(lhs, rhs)
  local blk = {}
  while #lhs >= 1 and #rhs > 1 do
    local head_lhs, head_rhs = table.remove(lhs, 1), table.remove(rhs, 1)
    table.insert(blk, build.store(head_lhs, head_rhs))
  end
  if #lhs >= 1 then -- #rhs <= 1
    if #rhs == 0 then
      -- set all to nil
      for _, v in ipairs(lhs) do
        table.insert(blk, build.store(v, build.nilval()))
      end
    else
      assert(#rhs == 1)
      if rhs[1][1] == "call" and #lhs > 1 then
        local call = rhs[1]
        call[1] = "call-to-multi"
        table.insert(call, 2, lhs)
        table.insert(blk, call)
      elseif rhs[1][1] == "dots" and #lhs > 1 then
        local stmt = lhs
        table.insert(stmt, 1, "dots-to-multi")
        table.insert(blk, stmt)
      else
        assert(#lhs >= 1)
        local head_lhs = table.remove(lhs, 1)
        assert(#head_lhs ~= 0)
        table.insert(blk, build.store(head_lhs, rhs[1]))
        -- set all others to nil
        for _, v in ipairs(lhs) do
          table.insert(blk, build.store(v, build.nilval()))
        end
      end
    end
  elseif #rhs > 1 then -- #lhs == 0
    assert(#lhs == 0)
    -- and throw them all away ... but we DO have to execute any side effects!
    for _, v in ipairs(rhs) do
      from_expr_to_side_effects(rhs, blk)
    end
  end
  return build.block(blk)
end

function build.expr_to_stmt(expr)
  if expr[1] == "call" then
    return expr
  else
    return nil
  end
end

function build.block(scope, stmts)
  --[[if #stmts == 1 then
    return stmts[1]
  end]]
  return {"do", scope, stmts}
end

local tbuild = {}

local ignore = {index=true, member=true, boolean=true, lookup=true, number=true, dots=true, table=true, string=true, binop=true, unop=true, nilval=true, call=true}

setmetatable(tbuild, {__index = function(table,key)
      local base = build[key]
      if ignore[key] then return base end
      return base and function(...) print("calling", "build." .. key) return base(...) end
    end})

return tbuild
