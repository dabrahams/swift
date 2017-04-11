// RUN: %target-swift-frontend %s -emit-ir -g -o %t.ll
// RUN: %FileCheck %s < %t.ll

// XFAIL: *
//
// This test is failing in the unicode-rethink branch; it seems to depend on
// implementation details of String

struct String { 
  var owner: AnyObject? = nil
  var start = 0, end = 0
}
class UIWindow {}
class AppDelegate {
  var window: UIWindow?
  // CHECK: define hidden {{.*}}i1 {{.*}}11AppDelegateC1f
  func f() -> Bool {
    // Test for -O0 shadow copies.
    // CHECK: call void @llvm.dbg.declare({{.*}}, metadata ![[SELF:.*]], metadata !{{[0-9]+}})
    // CHECK-NOT: call void @llvm.dbg.value
    // CHECK: call void @llvm.dbg.declare({{.*}}, metadata ![[A:.*]], metadata !{{[0-9]+}})
    let a = "let"
    // CHECK-NOT: call void @llvm.dbg.value
    // CHECK: call void @llvm.dbg.declare({{.*}}, metadata ![[B:.*]], metadata !{{[0-9]+}})
    // CHECK-NOT: call void @llvm.dbg.value
    // CHECK: ret
    // CHECK-DAG: ![[SELF]] = !DILocalVariable(name: "self", arg: 1{{.*}} line: [[@LINE-10]],
    // CHECK-DAG: ![[A]] = !DILocalVariable(name: "a",{{.*}} line: [[@LINE-6]],
    // CHECK-DAG: ![[B]] = !DILocalVariable(name: "b",{{.*}} line: [[@LINE+1]],
    var b = "var"
    self.window = UIWindow()
    return true
  }
}

// End-to-end test:
// RUN: llc %t.ll -filetype=obj -o %t.o
// RUN: %llvm-dwarfdump %t.o | %FileCheck %s --check-prefix DWARF-CHECK
// DWARF-CHECK: DW_AT_name {{.*}} "f"
//
// DWARF-CHECK: DW_TAG_formal_parameter
// DWARF-CHECK:  DW_AT_name {{.*}} "self"
//
// DWARF-CHECK:  DW_TAG_variable
// DWARF-CHECK:  DW_AT_name {{.*}} "a"
//
// DWARF-CHECK:  DW_TAG_variable
// DWARF-CHECK:  DW_AT_name {{.*}} "b"
