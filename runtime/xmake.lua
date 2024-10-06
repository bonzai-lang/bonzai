if is_plat("windows") then
  set_toolchains("clang-cl")
else 
  set_toolchains("clang")
end

set_warnings("allextra")

target("bonzai-runtime")
  add_rules("mode.release")
  add_files("src/**.c")
  add_includedirs("include")
  set_kind("binary") 
  set_targetdir("bin")
  set_optimize("fastest")

target("bonzai-runtime-test")
  add_rules("mode.debug", "mode.profile")
  add_files("src/**.c")
  add_includedirs("include")
  set_targetdir("bin")
  set_kind("binary")
  set_symbols("debug")
  add_cxflags("-pg")
  add_ldflags("-pg")

target("bonzai-library")
  add_rules("mode.release")
  add_files("src/**.c")
  add_includedirs("include")
  set_kind("static")
  set_targetdir("lib")
  set_optimize("fastest")
