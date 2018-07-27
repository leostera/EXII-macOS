//
//  Utils.swift
//  surface
//
//  Created by Leandro Ostera on 2018-07-24.
//

import Foundation

class Utils {
    static func now() -> String {
        // While Date().description gives us a nicely formatted date, it doesn't have enough precision
        return String(Int(Date().timeIntervalSince1970 * 100000))
    }
}
