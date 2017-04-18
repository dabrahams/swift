//===--- Migrator.h - Migrator ----------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// The top-level Swift Migrator driver.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_MIGRATOR_MIGRATOR_H
#define SWIFT_MIGRATOR_MIGRATOR_H

#include "swift/Migrator/MigrationState.h"
#include "swift/Migrator/Replacement.h"
#include "swift/Syntax/References.h"

namespace swift {
class CompilerInstance;

namespace migrator {

/// Run the migrator on the compiler invocation's input file and emit a
/// "replacement map" describing the requested changes to the source file.
bool updateCodeAndEmitRemap(CompilerInstance &Instance,
                            const CompilerInvocation &Invocation);

class Migrator {
  CompilerInstance &StartInstance;
  const CompilerInvocation &StartInvocation;
  SourceManager SrcMgr;
  std::vector<RC<MigrationState>> States;

public:
  Migrator(CompilerInstance &StartInstance,
           const CompilerInvocation &StartInvocation);

  /// The maximum number of times to run the compiler over the input to get
  /// fix-its. Nullability changes may expose other fix-its in subsequent
  /// compilations.
  static constexpr unsigned MaxCompilerFixitPassIterations = 7;

  /// Repeatedly perform a number of compielr-fix-it migrations in a row, until
  /// there are no new suggestions from the compiler or some other error
  /// occurred.
  void repeatFixitMigrations(const unsigned Iterations);

  /// Perform a single compiler fix-it migration on the last state, and push
  /// the result onto the state history.
  llvm::Optional<RC<FixitMigrationState>> performAFixItMigration();

  /// Starting with the last state, perform the following migration passes.
  llvm::Optional<RC<MigrationState>> performSyntacticPasses(/*, Array of passes */);

  /// Populate an array of replacements representing the difference between
  /// the start state's text and the end state's text.
  void getReplacements(SmallVectorImpl<Replacement> &Replacements) const;

  /// Emit the output text of the final state in States to the path specified
  /// by -emit-migrated-file-path in StartInvocation.MigratorOptions.
  ///
  /// Returns true if an attempt was made and failed.
  bool emitMigratedFile() const;

  /// Dump all of the migration states encountered so far to
  /// StartInvocation.MigratorOptions.DumpMigrationStatesDir.
  ///
  /// Returns true if an attempt was made and failed.
  bool dumpStates() const;

  /// Get the options for the Migrator.
  const MigratorOptions &getMigratorOptions() const;

  /// Get the filename of the input given by this->StartInvocation.
  const StringRef getInputFilename() const;
};

} // end namespace migrator
} // end namespace swift

#endif // SWIFT_MIGRATOR_MIGRATOR_H
