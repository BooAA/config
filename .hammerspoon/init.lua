require("hs.ipc")

-- hs.spaces.setDefaultMCwaitTime(0.3)
-- function myGotoSpace(space_num)
--    local screen = hs.screen.mainScreen()
--    local space_ids = hs.spaces.allSpaces()[screen:getUUID()]
--    hs.spaces.gotoSpace(space_ids[space_num])
-- end

function tileDir(dir)
   local window = hs.window.focusedWindow()
   if not window then
      return
   end

   local frame = window:screen():frame()
   if dir == "up" then
      frame.h = frame.h / 2
   elseif dir == "down" then
      frame.h = frame.h / 2
      frame.y = frame.y + frame.h
   elseif dir == "left" then
      frame.w = frame.w / 2
   elseif dir == "right" then
      frame.w = frame.w / 2
      frame.x = frame.x + frame.w
-- else
--    -- if not captured by above cases, it will set
--    -- to the size of whole screen (fill).
   end
   window:setFrame(frame)
end

