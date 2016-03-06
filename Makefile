#Basic makefile for Plutus

MONO=mono
COMPILER=fsharpc
FILES=Media.fs CoroutineMonad.fs Lens.fs Input.fs Math.fs Rendering.fs Mailbox.fs Networking.fs Logic.fs Game.fs Program.fs
REFERENCES=-r /usr/lib/monogame/MonoGame.Framework.dll -r /usr/lib/monogame/Lidgren.Network.dll

app.exe: ${FILES}
	${COMPILER} --nologo --target:exe --platform:x86 ${REFERENCES} -o $@ ${FILES}
