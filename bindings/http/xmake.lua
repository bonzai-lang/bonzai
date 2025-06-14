if is_plat("windows") then
  set_toolchains("clang-cl")
else 
  set_toolchains("clang")
end

set_warnings("allextra")

target("bonzai-http")
  add_rules("mode.release")
  add_files("src/**.c")
  add_files("../../runtime/src/**.c")
  add_includedirs("include")
  add_includedirs("../../runtime/include")

  add_cxflags("-Wall")

  set_kind("shared")
  set_targetdir("bin")
  set_optimize("fastest")
  set_basename("http")
  set_prefixname("")

target("bonzai-http-test")
  add_rules("mode.debug", "mode.profile")
  add_files("src/**.c")
  add_files("../../runtime/src/**.c")
  add_includedirs("include")
  add_includedirs("../../runtime/include")

  set_kind("shared")
  set_targetdir("bin")
  set_symbols("debug")
  add_cxflags("-pg")
  add_ldflags("-pg")
  set_basename("http-test")
  set_prefixname("")
