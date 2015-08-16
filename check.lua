parse = require "parse-lua"
util = require "util"

function readall(fname)
  local f = io.open(fname)
  local all = f:read("*a")
  f:close()
  return all
end

local succ, tbl = parse(readall("strict.lua"))

if not succ then
  print "Failed"
else
  print(util.PrintTable(tbl))
end
