-- Awesome configuration
-- Gaby Czegany <Glabsounet@gmail.com>

-- {{{1 Library
-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- User library
require("naughty")
local scratch = require("scratch")


-- {{{1 Variable definitions -- Themes define colours, icons, wallpapers and random definitions
beautiful.init(os.getenv("HOME") .. "/.config/awesome/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvtc"
editor = os.getenv("EDITOR") or "vi"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod1"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.spiral,
 
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
}

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}

-- Create a date popup
date = nil


-- {{{1 Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, s, layouts[1])
end


-- {{{1 Wibox
mysystray = widget({ type = "systray" })
mytextclock = awful.widget.textclock({ align = "right" }, "%H:%M ")

-- Separator
myspace = widget({ type = "textbox", name = "myspace", align = "right" })
myspace.text = " "

mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
                    )

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", height = 18, screen = s })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            mytaglist[s],
	    mylayoutbox[s],
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        mytextclock,
	myspace,
        s == 1 and mysystray or nil,
        layout = awful.widget.layout.horizontal.rightleft
    }
end


-- {{{1 Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "open terminal", terminal },
				    { "awesome", myawesomemenu }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })


-- {{{1 Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))

-- {{{1 Key bindings

-- {{{2 Global key
globalkeys = awful.util.table.join(
    -- Tags movement
    awful.key({ modkey,           }, "Left",	awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",	awful.tag.viewnext       ),
    awful.key({ modkey,           }, "`",	awful.tag.history.restore),

    -- Windows movement
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),

    -- Layout manipulation
    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end), 
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    -- Run prompt
    awful.key({ modkey,           }, "p", 
    function () 
	mypromptbox[mouse.screen].prompt = " Run: "
	mypromptbox[mouse.screen]:run() 
    end),

    -- Lua code prompt
    awful.key({ modkey, "Shift"   }, "p",
        function ()
	    awful.prompt.run({ prompt = " Run Lua code: " },
	    mypromptbox[mouse.screen].widget,
	    awful.util.eval, nil,
	    awful.util.getdir("cache") .. "/history_eval")
	end),

    -- Manual Prompt
    awful.key({ modkey }, "m", 
    function ()
	awful.prompt.run({ prompt = " Manual: " }, 
	mypromptbox[mouse.screen].widget,
	function (args) awful.util.spawn(terminal.. " -name Manual -e man " .. args) end)
    end)
)

-- {{{2 Client key
clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),

    awful.key({ modkey, "Shift"	  }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end),

    -- Sound Key
    awful.key({}, "XF86AudioRaiseVolume",   function () awful.util.spawn("amixer set Master 10%+")	end),
    awful.key({}, "XF86AudioLowerVolume",   function () awful.util.spawn("amixer set Master 10%-")	end),
    awful.key({}, "XF86AudioMute",	    function () awful.util.spawn("amixer set Master toggle")    end),

    -- Get date
    awful.key({ modkey,           }, "i",	
	function () if date ~= nil then del_date () 
		    else add_date () end end),

    -- Lock my screen
    awful.key({}, "XF86Sleep",		    function () awful.util.spawn("slock")    end),

    -- ScratchPad
    awful.key({ modkey,		  }, "s", 
	function () scratch.drop("urxvtc -name scratchdrop", "bottom", "center", 1, 0.30, true) end)
)

-- {{{2 Per tag key

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))
-- 2}}}

-- Set keys
root.keys(globalkeys)

-- {{{1 Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "Pidgin" },
      properties = { tag = tags[1][9] , floating = true } },
    { rule = { name = "plugin-container"},
      properties = { floating = true } },
    { rule = { name = "Wicd Network Manager" },
      properties = { floating = true } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    { rule = { class = "Aurora" },
      properties = { tag = tags[1][2] } },
    { rule = { name = "Aurora Preferences" },
      properties = { floating = true } },
    { rule = { class = "Firefox" },
      properties = { tag = tags[1][2] } },
    { rule = { name = "Firefox Preferences" },
      properties = { floating = true } },
}

-- {{{1 Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)


-- {{{1 Notification
naughty.config.default_preset.timeout          = 5
naughty.config.default_preset.screen           = 1
naughty.config.default_preset.position         = "top_right"
naughty.config.default_preset.gap              = 1
naughty.config.default_preset.ontop            = true
naughty.config.default_preset.font             = beautiful.font or "Inconsolata 8"
naughty.config.default_preset.icon             = nil
naughty.config.default_preset.hover_timeout    = nil

-- {{{2 Popup date

function del_date()
    if date ~= nil then
	naughty.destroy(date)
	date = nil
    end
end

function add_date()
    date =  naughty.notify ({
	    text = string.format( os.date("%a, %d %B %Y")),
	    position = "top_right",

    })
end

-- change clockbox for your clock widget (e.g. mytextclock)
mytextclock:add_signal("mouse::enter", del_date)
mytextclock:add_signal("mouse::enter", add_date)
mytextclock:add_signal("mouse::leave", del_date)


-- {{{1 Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.add_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end

-- A small debuger using naughty
function dbg(vars)
    local text = ""
    for i=1, #vars do text = text .. vars[i] .. " | " end
    naughty.notify({ text = text, timeout = 0 })
end


-- {{{1 Testing

