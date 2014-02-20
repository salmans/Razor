-- Access Control Policies:
ip(ipsrc) & port(portsrc) & ip00(ipdest) & port00(portdest) => Accept(ipsrc,ipdest,portsrc,portdest)
ip(ipsrc) & port(portsrc) & ip01(ipdest) & port01(portdest) => Accept(ipsrc,ipdest,portsrc,portdest)
ip(ipsrc) & port(portsrc) & ip02(ipdest) & port00(portdest) => Accept(ipsrc,ipdest,portsrc,portdest)
ip(ipsrc) & port(portsrc) & ip03(ipdest) & port01(portdest) => Accept(ipsrc,ipdest,portsrc,portdest)
-- Query:
Accept(ipsrc,ipdest,portsrc,portdest) => (ip(ipsrc) & port(portsrc) & ip00(ipdest) & port00(portdest)) | (ip(ipsrc) & port(portsrc) & ip01(ipdest) & port01(portdest)) | (ip(ipsrc) & port(portsrc) & ip02(ipdest) & port00(portdest)) | (ip(ipsrc) & port(portsrc) & ip03(ipdest) & port01(portdest))
exists x.exists y.exists z. exists w.Accept(x,y,z,w)