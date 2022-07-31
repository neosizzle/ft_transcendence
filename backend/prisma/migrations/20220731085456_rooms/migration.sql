-- CreateTable
CREATE TABLE "user_rooms" (
    "id" SERIAL NOT NULL,
    "RoomID" INTEGER NOT NULL,
    "UserID" INTEGER NOT NULL,

    CONSTRAINT "user_rooms_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "room" (
    "RoomID" SERIAL NOT NULL,
    "RoomName" TEXT NOT NULL,

    CONSTRAINT "room_pkey" PRIMARY KEY ("RoomID")
);
