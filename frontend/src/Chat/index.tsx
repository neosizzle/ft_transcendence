import React, { useState, useEffect } from "react";
import {
  API_ROOT,
  INCOMING_BAN,
  INCOMING_DEMOTION,
  INCOMING_OWNER_TRANSFER,
  INCOMING_PROMOTION,
  NOT_FRIENDS,
} from "../constants";
import { useAuth, User } from "../context/authContext";
import { auth_net_get } from "../utils";
import { ERR, INCOMING_KICK, INCOMING_MSG } from "../constants";
import { Admin, Ban, BaseWSResponse, Member, Message, Room } from "./classes";
import { useNavigate, useSearchParams } from "react-router-dom";
import { useChat } from "../context/chatContext";
import RoomList from "./components/RoomList";
import ChatArea from "./components/ChatArea";
import MemberList from "./components/MemberList";
import Alert from "../commonComponents/Alert";
import { cloneDeep } from "lodash";

const chatEndpoint = `${API_ROOT}/chat`;
const memberEndpoint = `${API_ROOT}/members`;
const ROOM_PAGE_SIZE = 5;

function Chat() {
  const auth = useAuth();
  const navigate = useNavigate();
  const chat = useChat();
  const [searchParams] = useSearchParams();

  // DM STATES
  useState<number>(NOT_FRIENDS);
  const [otherDmUser, setOtherDmUser] = useState<User | null>(null);

  // handle user leave / getting kicked
  const handleKick = (data: Member) => {
    if (data.userId !== auth?.user?.id) return;

    // if kicked user is me and im in active room, set active room to next room and remove current room from rooms
    if (data.roomId === chat?.activeRoomRef?.current?.id) {
      let roomsClone = cloneDeep(chat.roomsRef.current as Room[]);
      roomsClone = roomsClone.filter((room) => room.id !== data.roomId);

      chat.setActiveRoom(roomsClone[0]);
      chat.setRooms(roomsClone);
      return;
    }

    // if kicked user is me and im on one of non active rooms, remove room from rooms
    if (
      chat?.roomsRef &&
      chat?.roomsRef?.current?.findIndex((room) => room.id === data.roomId)
    ) {
      let roomsClone = cloneDeep(chat.roomsRef.current as Room[]);
      roomsClone = roomsClone.filter((room) => room.id !== data.roomId);

      chat.setRooms(roomsClone);
      return;
    }
  };

  // handle new incoming message
  const handleNewMessage = (data: Message) => {
    // add message into active room data if room id is active room id
    if (data.roomId === chat?.activeRoomRef.current?.id) {
      const activeRoomMessagesClone = cloneDeep(
        chat?.activeRoomMessagesRef.current as Message[]
      );
      activeRoomMessagesClone.push(data);

      chat?.setActiveRoomMessages(activeRoomMessagesClone);
      chat.setActiveRoomMessagesCount(chat.activeRoomMessagesCount + 1);
    }

    // add room if data room is not in active room (need to work with edison for scrolling)
    const roomIds: number[] | undefined = chat?.roomsRef.current?.map(
      (room) => room.id
    );
    if (!roomIds || !roomIds.includes(data.roomId)) {
      const roomClone = cloneDeep(chat?.roomsRef.current || []);
      roomClone.push(data.room);
      chat?.setRooms(roomClone);
    }
  };

  // handle error event
  const handleError = (data: BaseWSResponse) => {
    if (data.message === "Forbidden") navigate("/logout");
    else {
      if (data.message) chat?.setAlertMessage(data.message);
      else chat?.setAlertMessage("Unexpected error");
      chat?.setOpenAlert({ type: "error", isOpen: true });
    }
  };

  // handle han event
  const handleBan = (data: Ban) => {
    // if banned user is not self, check for active room and update members and member users
    if (
      data.userId !== auth?.user?.id &&
      chat?.activeRoomRef.current?.id === data.roomId
    ) {
      let membersClone = cloneDeep(chat.membersRef.current as Member[]);
      let memberUsersClone = cloneDeep(chat.memberUsersRef.current as User[]);

      memberUsersClone = memberUsersClone.filter(
        (user) => user.id !== data.userId
      );
      membersClone = membersClone.filter(
        (member) => member.userId !== data.userId
      );
      chat.setMembers(membersClone);
      chat.setMemberUsers(memberUsersClone);
    }

    // if banned user is self, filter rooms and set active room to filtered first room
    if (data.userId === auth?.user?.id) {
      const memberToRemove: Member = {
        id: -1,
        userId: data.userId,
        roomId: data.roomId,
        room: data.room,
        user: data.user,
      };
      handleKick(memberToRemove);
    }
  };

  //handle admin promotion
  const handlePromotion = (data: Admin) => {
    // if active room is data room, update current room admins
    if (chat?.activeRoomRef.current?.id !== data.roomId) return;

    const adminsClone = cloneDeep(chat.adminsRef.current as Admin[]);
    adminsClone.push(data);
    chat.setAdmins(adminsClone);
  };

  // handle admin demotion
  const handleDemotion = (data: Admin) => {
    // if active room is data room, update current room admins
    if (chat?.activeRoomRef.current?.id !== data.roomId) return;

    let adminsClone = cloneDeep(chat.adminsRef.current as Admin[]);
    adminsClone = adminsClone.filter((admin) => admin.userId !== data.userId);
    chat.setAdmins(adminsClone);
  };

  // handle owner change
  const handleOwnerChange = (data: Room) => {
    // update curr room owner if data room is same as active room
    if (data.id !== chat?.activeRoomRef.current?.id) return;

    const roomCpy: Room = {
      id: chat.activeRoomRef.current.id,
      roomName: chat.activeRoomRef.current.roomName,
      ownerId: data.ownerId,
      type: chat.activeRoomRef.current.type,
      isProtected: chat.activeRoomRef.current.isProtected,
      createdAt: chat.activeRoomRef.current.createdAt,
      updatedAt: chat.activeRoomRef.current.updatedAt,
    };
    chat.setActiveRoom(roomCpy);
  };

  useEffect(() => {
    auth_net_get(
      `${memberEndpoint}?page=1&pageSize=${ROOM_PAGE_SIZE}&filterOn=userId&filterBy=${auth?.user?.id}`
    ).then((data) => {
      const roomsArr: Room[] = data.data.map((e: Member) => e.room);
      chat?.setRooms(roomsArr);
      chat?.setActiveRoomCount(data.total_elements);
    });
    if (!auth?.chatSocket) return;
    auth.chatSocket.on(INCOMING_MSG, handleNewMessage);
    auth.chatSocket.on(ERR, handleError);
    auth.chatSocket.on(INCOMING_KICK, handleKick);
    auth.chatSocket.on(INCOMING_BAN, handleBan);
    auth.chatSocket.on(INCOMING_PROMOTION, handlePromotion);
    auth.chatSocket.on(INCOMING_DEMOTION, handleDemotion);
    auth.chatSocket.on(INCOMING_OWNER_TRANSFER, handleOwnerChange);

    return () => {
      // remove socket listeners
      auth?.chatSocket?.off(INCOMING_MSG, handleNewMessage);
      auth?.chatSocket?.off(ERR, handleError);
      auth?.chatSocket?.off(INCOMING_KICK, handleKick);
      auth?.chatSocket?.off(INCOMING_BAN, handleBan);
      auth?.chatSocket?.off(INCOMING_PROMOTION, handlePromotion);
      auth?.chatSocket?.off(INCOMING_DEMOTION, handleDemotion);
      auth?.chatSocket?.off(INCOMING_OWNER_TRANSFER, handleOwnerChange);
    };
  }, [auth]);

  useEffect(() => {
    //For first time setting activeRoomMessages
    if (chat?.rooms != null) {
      const initRoomIdSelect = searchParams.get("room");
      if (initRoomIdSelect) {
        const initRoom = chat.rooms.find(
          (room) => room.id === parseInt(initRoomIdSelect, 10)
        );
        if (initRoom) chat.setActiveRoom(initRoom);
        else chat.setActiveRoom(chat.rooms[0]);
      } else chat.setActiveRoom(chat.rooms[0]);
    }
  }, [chat?.rooms]);

  useEffect(() => {
    //For setting activeRoomMessages
    if (chat?.activeRoom != null) {
      auth_net_get(
        `${chatEndpoint}?page=1&pageSize=15&filterOn=roomId&filterBy=${chat.activeRoom.id}&sortBy=Descending&sortOn=createdAt`
      ).then((data) => {
        // token expired
        if (data.error && data.error == "Forbidden") return navigate("/logout");

        const chats = data.data;
        const msgsArr: Message[] = [];
        chats.forEach((chat: Message) => {
          msgsArr.unshift(chat);
        });
        chat?.setActiveRoomMessages(msgsArr);
        chat.setActiveRoomMessagesCount(data.total_elements);
      });
      //Gets the members for display on the right
      auth_net_get(
        `${memberEndpoint}?page=1&pageSize=50&filterOn=roomId&filterBy=${chat.activeRoom.id}`
      ).then((data) => {
        // token expired
        if (data.error && data.error == "Forbidden") return navigate("/logout");

        const member = data.data;
        const userArr: User[] = [];

        member.forEach((member: Member) => {
          userArr.push(member.user);
        });
        chat?.setMembers(member);
        chat?.setMemberUsers(userArr);

        // set dm states
        if (chat?.activeRoom?.type !== "DM") return;

        const otherUser =
          userArr[0].id === auth?.user?.id ? userArr[1] : userArr[0];
        setOtherDmUser(otherUser);
      });
    }
  }, [chat?.activeRoom]);

  return (
    <>
      {/* Header */}
      <h1 className="text-5xl">Chat</h1>
      <div className="grid grid-cols-3">
        {/* Need to figure out how to get user avatar. If DM, get user.avatar as src string. Else if GC, use default? (Alternatively
						can use owner picture) */}

        {/* Room list */}
        <RoomList />

        {/* Chat area */}
        <ChatArea
          members={chat ? chat.members : null}
          memberUsers={chat ? chat.memberUsers : null}
          otherDmUser={otherDmUser}
        />

        {/* Members list */}
        <MemberList memberUsers={chat ? chat.memberUsers : null} />

        {/* Alert Box */}
        {chat?.openAlert.isOpen ? (
          <Alert
            setOpenAlert={chat.setOpenAlert}
            message={chat.alertMessage}
            alert={chat.openAlert}
          />
        ) : null}
      </div>
    </>
  );
}

export default Chat;
