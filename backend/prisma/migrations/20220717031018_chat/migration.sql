-- CreateTable
CREATE TABLE "Chat" (
    "id" SERIAL NOT NULL,
    "UserID" INTEGER NOT NULL,
    "timestamp" TIMESTAMP(3) NOT NULL,
    "message" TEXT NOT NULL,

    CONSTRAINT "Chat_pkey" PRIMARY KEY ("id")
);
