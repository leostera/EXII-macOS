//
//  SocketServer.swift
//  surface
//
//  Created by Leandro Ostera on 2018-07-28.
//

import Foundation

enum SocketError: Error {
    case CouldNotUnwrapListenerSocket
}

class SocketServer {
    var socket: Socket?
    var listener: Socket
    let path = NSString(string: "/tmp/surface.sock").expandingTildeInPath
    var buffer_size: Int = 4096

    init() throws {
        try listener = Socket.create(family: .unix)
        try listener.listen(on: path, maxBacklogSize: Socket.SOCKET_DEFAULT_MAX_BACKLOG)
        print("Listening on path: \(path)")
    }

    @objc func onCoordinateData(notification: NSNotification) {
        guard let nobj = notification.object as? NSDictionary else {
            return
        }
        guard let data: EXIICoordinateData = nobj["data"] as? EXIICoordinateData else {
            return
        }
        print(data.asByteArray)
        try? socket?.write(from: Data.init(bytes: data.asByteArray))
    }

    @objc func run() {
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(self.onCoordinateData(notification:)),
            name: .EXIICoordinateData,
            object: nil
        )
        
        RunLoop.current.run()
    }
    
    func start() {
        let daemon = Thread(
            target: self,
            selector: #selector(self.run),
            object: nil
        )
        
        daemon.start()
        
        print(Utils.now(), "Started UNIX Socket Server...")
    }
}
