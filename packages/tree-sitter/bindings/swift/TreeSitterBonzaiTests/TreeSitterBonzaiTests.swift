import XCTest
import SwiftTreeSitter
import TreeSitterBonzai

final class TreeSitterBonzaiTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_bonzai())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Bonzai grammar")
    }
}
