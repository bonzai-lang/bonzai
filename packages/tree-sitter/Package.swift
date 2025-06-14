// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "TreeSitterBonzai",
    products: [
        .library(name: "TreeSitterBonzai", targets: ["TreeSitterBonzai"]),
    ],
    dependencies: [
        .package(url: "https://github.com/ChimeHQ/SwiftTreeSitter", from: "0.8.0"),
    ],
    targets: [
        .target(
            name: "TreeSitterBonzai",
            dependencies: [],
            path: ".",
            sources: [
                "src/parser.c",
                // NOTE: if your language has an external scanner, add it here.
            ],
            resources: [
                .copy("queries")
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
        .testTarget(
            name: "TreeSitterBonzaiTests",
            dependencies: [
                "SwiftTreeSitter",
                "TreeSitterBonzai",
            ],
            path: "bindings/swift/TreeSitterBonzaiTests"
        )
    ],
    cLanguageStandard: .c11
)
