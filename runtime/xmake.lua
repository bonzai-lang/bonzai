if is_plat("windows") then
  set_toolchains("clang-cl")
else 
  set_toolchains("clang")
end

set_warnings("allextra")

target("bonzai-runtime")
  -- add_rules("mode.release")

  -- Adding the runtime source files
  add_files("src/**.c")
  add_includedirs("include")

  add_cxflags("-Wall")

  -- Setting up the target
  set_kind("binary") 
  set_targetdir("bin")
  set_optimize("fast")

target("bonzai-runtime-test")
  add_rules("mode.debug", "mode.profile")

  -- Adding the runtime source files
  add_files("src/**.c")
  add_includedirs("include")
  
  -- Setting up the target  
  set_targetdir("bin")
  set_kind("binary")
  set_symbols("debug")
  set_policy("build.sanitizer.address", true)
  add_cxflags("-pg")
  add_ldflags("-pg")

target("bonzai-library")
  add_rules("mode.release")
  
  -- Adding the runtime source files
  add_files("src/**.c")
  add_includedirs("include")

  -- Setting up the target
  set_kind("static")
  set_targetdir("lib")
  set_optimize("fastest")
