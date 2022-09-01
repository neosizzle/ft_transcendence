import React, { useState, useEffect } from "react";
import { API_ROOT, NOT_FRIENDS } from "../constants";
import { useAuth, User } from "../context/authContext";
import { auth_net_get } from "../utils";
import { ERR, INCOMING_KICK, INCOMING_MSG } from "../constants";
import { BaseWSResponse, Member, Message, Room } from "./classes";
import { useNavigate, useSearchParams } from "react-router-dom";
import { useChat } from "../context/chatContext";
import RoomList from "./components/RoomList";
import ChatArea from "./components/ChatArea";
import MemberList from "./components/MemberList";
import Alert from "../commonComponents/Alert";
import { cloneDeep } from "lodash";

const chatEndpoint = `${API_ROOT}/chat`;
const memberEndpoint = `${API_ROOT}/members`;

function Chat() {
  const auth = useAuth();
  const navigate = useNavigate();
  const chat = useChat();
  const [searchParams, setSearchParams] = useSearchParams();
  const [memberUsers, setMemberUsers] = useState<User[] | null>(null); // user objects for members
  const [members, setMembers] = useState<Member[] | null>(null); // member objects for active room

  // DM STATES
  useState<number>(NOT_FRIENDS);
  const [otherDmUser, setOtherDmUser] = useState<User | null>(null);

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

  const handleNewMessage = (data: Message) => {
    // add message into active room data if room id is active room id
    if (data.roomId === chat?.activeRoomRef.current?.id) {
      const activeRoomMessagesClone = cloneDeep(
        chat?.activeRoomMessagesRef.current as Message[]
      );
      activeRoomMessagesClone.push(data);

      chat?.setActiveRoomMessages(activeRoomMessagesClone);
      chat.setActiveRoomMessagesCount(chat.activeRoomMessagesCount + 1)
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

  const handleError = (data: BaseWSResponse) => {
    if (data.message === "Forbidden") navigate("/logout");
    else {
      if (data.message) chat?.setAlertMessage(data.message);
      else chat?.setAlertMessage("Unexpected error");
      chat?.setOpenAlert({ type: "error", isOpen: true });
    }
  };

  useEffect(() => {
    auth_net_get(
      `${memberEndpoint}?page=1&pageSize=50&filterOn=userId&filterBy=${auth?.user?.id}`
    ).then((data) => {
      const roomsArr: Room[] = data.data.map((e: Member) => e.room);
      chat?.setRooms(roomsArr);
    });
    if (!auth?.chatSocket) return;
    auth.chatSocket.on(INCOMING_MSG, handleNewMessage);
    auth.chatSocket.on(ERR, handleError);
    auth.chatSocket.on(INCOMING_KICK, handleKick);
    // auth.chatSocket.on(INCOMING_BAN, Test);

    return () => {
      // remove socket listeners
      auth?.chatWidgetSocket?.off(INCOMING_MSG, handleNewMessage);
      auth?.chatWidgetSocket?.off(ERR, handleError);
      auth?.chatWidgetSocket?.off(INCOMING_KICK, handleKick);
      // auth?.chatWidgetSocket?.off(INCOMING_BAN, Test);
    };
  }, [auth]);

  useEffect(() => {
    //For first time setting activeRoomMessages
    if (chat?.rooms != null) {
      const initRoomIdSelect = searchParams.get("room");
      if (initRoomIdSelect)
      {
        const initRoom = chat.rooms.find((room) => room.id === parseInt(initRoomIdSelect, 10))
        if (initRoom) chat.setActiveRoom(initRoom)
        else chat.setActiveRoom(chat.rooms[0]);
      }
      else chat.setActiveRoom(chat.rooms[0]);
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
        setMembers(member);
        setMemberUsers(userArr);

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
          members={members}
          memberUsers={memberUsers}
          otherDmUser={otherDmUser}
        />

        {/* Members list */}
        <MemberList memberUsers={memberUsers} />

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
