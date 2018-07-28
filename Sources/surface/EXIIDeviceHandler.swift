//
//  Surface.swift
//  surface
//
//  Created by Leandro Ostera on 2018-07-25.
//

import Foundation

class EXIIDeviceHandler {
    var devices: [UInt64: EXIIDevice] = [:]
    
    @objc func onConnection(notification: NSNotification) {
        guard let nobj = notification.object as? NSDictionary else {
            return
        }
        guard let deviceInfo: USBDevice = nobj["device"] as? USBDevice else {
            return
        }
        let device = EXIIDevice(deviceInfo)
        
        // Reset incoming device
        try! device.reset()
//        try! device.calibrate()
        
        devices.updateValue(device, forKey: deviceInfo.deviceId)
        print(Utils.now(), "Registered EXII Device", deviceInfo.deviceId)
    }
    
    @objc func onDisconnection(notification: NSNotification) {
        guard let nobj = notification.object as? NSDictionary else {
            return
        }
        guard let deviceId: UInt64 = nobj["id"] as? UInt64 else {
            return
        }
        devices.removeValue(forKey: deviceId)
        print(Utils.now(), "Unregistered EXII Device", deviceId)
    }
    
    @objc func run() {
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(self.onConnection(notification:)),
            name: .USBDeviceConnected,
            object: nil
        )

        NotificationCenter.default.addObserver(
            self,
            selector: #selector(self.onDisconnection(notification:)),
            name: .USBDeviceDisconnected,
            object: nil
        )
        
        while true {
            for d in devices {
                do {
                    let data = try d.value.coordinate()!
                    NotificationCenter.default.post(name: .EXIICoordinateData, object: [ "data": data ])
                } catch {
                    
                }
            }
        }
    }
    
    func start() {
        let daemon = Thread(
            target: self,
            selector:#selector(self.run),
            object: nil)
        
        daemon.start()
        
        print(Utils.now(), "Started EXII DeviceHandler Daemon.")
    }
}
