# CMake generated Testfile for 
# Source directory: /Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2
# Build directory: /Users/manuelmaly/Dev/ai/git-jenga/zig-cache/libgit2/tests/libgit2
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(offline "/Users/manuelmaly/Dev/ai/git-jenga/zig-cache/libgit2/libgit2_tests" "-v" "-xonline")
set_tests_properties(offline PROPERTIES  _BACKTRACE_TRIPLES "/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/cmake/AddClarTest.cmake;7;add_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;70;add_clar_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;0;")
add_test(invasive "/Users/manuelmaly/Dev/ai/git-jenga/zig-cache/libgit2/libgit2_tests" "-v" "-sfilter::stream::bigfile" "-sodb::largefiles" "-siterator::workdir::filesystem_gunk" "-srepo::init" "-srepo::init::at_filesystem_root" "-sonline::clone::connect_timeout_default")
set_tests_properties(invasive PROPERTIES  _BACKTRACE_TRIPLES "/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/cmake/AddClarTest.cmake;7;add_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;71;add_clar_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;0;")
add_test(online "/Users/manuelmaly/Dev/ai/git-jenga/zig-cache/libgit2/libgit2_tests" "-v" "-sonline" "-xonline::customcert")
set_tests_properties(online PROPERTIES  _BACKTRACE_TRIPLES "/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/cmake/AddClarTest.cmake;7;add_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;72;add_clar_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;0;")
add_test(online_customcert "/Users/manuelmaly/Dev/ai/git-jenga/zig-cache/libgit2/libgit2_tests" "-v" "-sonline::customcert")
set_tests_properties(online_customcert PROPERTIES  _BACKTRACE_TRIPLES "/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/cmake/AddClarTest.cmake;7;add_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;73;add_clar_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;0;")
add_test(gitdaemon "/Users/manuelmaly/Dev/ai/git-jenga/zig-cache/libgit2/libgit2_tests" "-v" "-sonline::push")
set_tests_properties(gitdaemon PROPERTIES  _BACKTRACE_TRIPLES "/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/cmake/AddClarTest.cmake;7;add_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;74;add_clar_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;0;")
add_test(gitdaemon_namespace "/Users/manuelmaly/Dev/ai/git-jenga/zig-cache/libgit2/libgit2_tests" "-v" "-sonline::clone::namespace")
set_tests_properties(gitdaemon_namespace PROPERTIES  _BACKTRACE_TRIPLES "/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/cmake/AddClarTest.cmake;7;add_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;75;add_clar_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;0;")
add_test(gitdaemon_sha256 "/Users/manuelmaly/Dev/ai/git-jenga/zig-cache/libgit2/libgit2_tests" "-v" "-sonline::clone::sha256")
set_tests_properties(gitdaemon_sha256 PROPERTIES  _BACKTRACE_TRIPLES "/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/cmake/AddClarTest.cmake;7;add_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;76;add_clar_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;0;")
add_test(ssh "/Users/manuelmaly/Dev/ai/git-jenga/zig-cache/libgit2/libgit2_tests" "-v" "-sonline::push" "-sonline::clone::ssh_cert" "-sonline::clone::ssh_with_paths" "-sonline::clone::path_whitespace_ssh" "-sonline::clone::ssh_auth_methods")
set_tests_properties(ssh PROPERTIES  _BACKTRACE_TRIPLES "/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/cmake/AddClarTest.cmake;7;add_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;77;add_clar_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;0;")
add_test(proxy "/Users/manuelmaly/Dev/ai/git-jenga/zig-cache/libgit2/libgit2_tests" "-v" "-sonline::clone::proxy")
set_tests_properties(proxy PROPERTIES  _BACKTRACE_TRIPLES "/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/cmake/AddClarTest.cmake;7;add_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;78;add_clar_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;0;")
add_test(auth_clone "/Users/manuelmaly/Dev/ai/git-jenga/zig-cache/libgit2/libgit2_tests" "-v" "-sonline::clone::cred")
set_tests_properties(auth_clone PROPERTIES  _BACKTRACE_TRIPLES "/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/cmake/AddClarTest.cmake;7;add_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;79;add_clar_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;0;")
add_test(auth_clone_and_push "/Users/manuelmaly/Dev/ai/git-jenga/zig-cache/libgit2/libgit2_tests" "-v" "-sonline::clone::push" "-sonline::push")
set_tests_properties(auth_clone_and_push PROPERTIES  _BACKTRACE_TRIPLES "/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/cmake/AddClarTest.cmake;7;add_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;80;add_clar_test;/Users/manuelmaly/Dev/ai/git-jenga/deps/libgit2/tests/libgit2/CMakeLists.txt;0;")
