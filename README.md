<img src="assets/lentil.png" alt="Lentil Logo" width="400" style="border-radius: 10px;">

> *Fault-tolerant chat backend • Built with Gleam & OTP*

```
⚠️  UNDER ACTIVE DEVELOPMENT • THINGS MAY BREAK  ⚠️
```

[![Docs](https://img.shields.io/badge/docs-hexdocs-ff69b4?style=flat-square)](https://hexdocs.pm/lentil/)
[![Gleam](https://img.shields.io/badge/gleam-✨-ffaff3?style=flat-square)](https://gleam.run)
[![OTP](https://img.shields.io/badge/otp-⚡-blue?style=flat-square)](https://www.erlang.org/doc/design_principles/des_princ.html)

---

## ⚡ **Quick Start**

```bash
git clone https://github.com/yourorg/lentil.git && cd lentil
gleam run  # → localhost:4000
```

> Set `PORT` & `DATABASE_URL` via environment variables

---

## 🎯 **API**

```
POST   /auth/signin           # Get session cookie
POST   /auth/signout          # Clear session
WS     /rooms/:id/ws          # Real-time chat
REST   /rooms                 # CRUD operations
```

**[→ Full Docs coming soon](https://hexdocs.pm/lentil/)**

---

## 🧪 **Development**

```bash
gleam run    # Start server
gleam test   # Run tests
```

---

## ✨ **Features**

🏗️ **OTP Actors** • Each user/room runs in supervised processes  
🔄 **Fault Recovery** • Supervisor trees restart crashed sessions  
🔌 **Hybrid API** • REST + WebSocket combo  
📈 **Horizontally Scalable** • Multi-node BEAM ready  

---

## 🤝 **Contributing**

1. Fork → Branch → Code → Test
2. `git commit -m "Add cool feature"`
3. Push → PR → 🎉

---

*Built with ❤️ using Gleam + OTP*
