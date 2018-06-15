// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

extension Collection {
  func foo() {
    // CHECK: witness_method $Self.Indices, #_Sequence.dropFirst!1 : <Self where Self : _Sequence> (Self) -> (Int) -> Self.SubSequence : $@convention(witness_method: _Sequence) <τ_0_0 where τ_0_0 : _Sequence> (Int, @in_guaranteed τ_0_0) -> @out τ_0_0.SubSequence
    _ = zip(indices, indices.dropFirst(3))
  }
}
