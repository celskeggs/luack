-- based on https://github.com/stravant/LuaMinify/blob/master/Scope.lua

local Scope = {
	new = function(self, parent)
		local s = {
			Parent = parent,
			Locals = { },
			Children = { },
		}
		if parent then
			table.insert(parent.Children, s)
		end
		return setmetatable(s, { __index = self })
	end,
  
  global = function(self, name)
    return {Scope=self, RealScope=self, Name=name, IsGlobal=true}
  end,
  
  Print = function(self)
    return "<Scope>"
  end,

	CreateLocal = function(self, name)
		local v, n = self:GetLocal(name)
		if v then return v, n end
		v = {Scope=self, RealScope=self, Name=name, IsGlobal=false, Layer=0}
    self.Locals[name] = v
		return v
	end,

	GetLocal = function(self, name)
    if not self.Locals[name] and self.Parent then
      local loc = self.Parent:GetLocal(name)
      if loc then
        self.Locals[name] = {Scope=self, RealScope=loc.RealScope, Name=name, IsGlobal=false, Layer=loc.Layer + 1}
      end
    end
    return self.Locals[name]
	end,
}

return Scope
