import React, { FunctionComponent, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { API_ROOT } from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { useChat } from "../../context/chatContext";
import { auth_net_get } from "../../utils";
import { Member } from "../classes";
import ConfirmationModal from "./ConfirmationModal";
import MemberCard from "./MemberCard";
import TimeSelectionModal from "./TimeSelectionModal";

const adminEndpoint = `${API_ROOT}/admins`;
const blocksEndpoint = `${API_ROOT}/blocks`;
const friendsEndpoint = `${API_ROOT}/friends`;
const memberEndpoint = `${API_ROOT}/members`;
const MEMBER_PAGE_SIZE = 10;

interface MemberListProps {
  memberUsers: User[] | null;
}

const MemberList: FunctionComponent<MemberListProps> = ({ memberUsers }) => {
  const chat = useChat();
  const auth = useAuth();
  const navigate = useNavigate();
  const [currChatPage, setCurrChatPage] = useState<number>(1);
  const [hitBtm, setHitBtm] = useState<boolean>(true);

  // initialize member block and friend values
  useEffect(() => {
    // evaluate blocks
    auth_net_get(
      `${blocksEndpoint}?page=1&pageSize=1`
    ).then(async (data) => {
      // token expired
      if (data.error && data.error == "Forbidden") return navigate("/logout");

      if (!data.total_elements) return;
      const blocks = await auth_net_get(
        `${blocksEndpoint}?page=1&pageSize=${data.total_elements}`
      );
      chat?.setActiveRoomBlocks(blocks.data);
    });

    // evaluate friends
    auth_net_get(`${friendsEndpoint}?page=1&pageSize=1`).then(async (data) => {
      // token expired
      if (data.error && data.error == "Forbidden") return navigate("/logout");

      if (!data.total_elements) return;
      const friends = await auth_net_get(
        `${friendsEndpoint}?page=1&pageSize=${data.total_elements}`
      );
      chat?.setActiveRoomFriends(friends.data);
    });
  }, [memberUsers]);

  // initialize Gc specific values
  useEffect(() => {
    // return if dm
    if (!chat || chat?.activeRoom?.type !== "GC") return;

    // evaluate all admins
    // evaluate total number of admins in room
    auth_net_get(
      `${adminEndpoint}?page=1&pageSize=1&filterOn=roomId&filterBy=${chat.activeRoom.id}`
    ).then(async (data) => {
      if (data.error && data.error == "Forbidden") return navigate("/logout");

      // evaluate all admins in room
      if (!data.total_elements) return;
      const adminsRes = await auth_net_get(
        `${adminEndpoint}?page=1&pageSize=${data.total_elements}&filterOn=roomId&filterBy=${chat?.activeRoom?.id}`
      );
      chat.setAdmins(adminsRes.data);
    });
  }, [memberUsers]);

  const handleScroll = (e: React.UIEvent<HTMLDivElement>) => {
    if (!chat) return;
    if (e.currentTarget.scrollTop === 0) {
      if (currChatPage === 1) return;
      setHitBtm(false)
      setCurrChatPage(currChatPage - 1);
      auth_net_get(
        `${memberEndpoint}?page=${currChatPage - 1}&pageSize=${MEMBER_PAGE_SIZE}&filterOn=roomId&filterBy=${chat.activeRoom?.id}`
      ).then((data) => {
        if (data.error && data.error == "Forbidden") return navigate("/logout");
        const member = data.data;
        const userArr: User[] = [];
        member.forEach((member: Member) => {
          userArr.push(member.user);
        });
        chat.setMembers(member);
        chat.setMemberUsers(userArr);
      });
    }
    else if (Math.floor(e.currentTarget.scrollHeight - e.currentTarget.scrollTop) === e.currentTarget.clientHeight) {
      if (chat.memberCount/ MEMBER_PAGE_SIZE < 1 || currChatPage >= chat.memberCount / MEMBER_PAGE_SIZE)
        return;
      setHitBtm(true)
      setCurrChatPage(currChatPage + 1);
      auth_net_get(
        `${memberEndpoint}?page=${currChatPage + 1}&pageSize=${MEMBER_PAGE_SIZE}&filterOn=roomId&filterBy=${chat.activeRoom?.id}`
      ).then((data) => {
        if (data.error && data.error == "Forbidden") return navigate("/logout");
        const member = data.data;
        const userArr: User[] = [];
        member.forEach((member: Member) => {
          userArr.push(member.user);
        });
        chat.setMembers(member);
        chat.setMemberUsers(userArr);
      });
    }
  }

  useEffect(() => {
    if (hitBtm){
      const ref = document.getElementById("top-members");
      ref?.scrollIntoView();
    }
    else {
      const ref = document.getElementById("bottom-members");
      ref?.scrollIntoView({block: "end"});
    }
  }, [chat?.memberUsers]);

  return (
    <div>
      <div className="border-2 text-center text-5xl">Members</div>

      <div className="h-96 overflow-scroll" onScroll={handleScroll}>
      <div className="my-3 py-3" id="top-members"></div>
        <div>
          {chat && memberUsers?.map((user) => (
            <MemberCard
              userBlocked={
                (chat?.activeRoomBlocks?.findIndex(
                  (block) => block.blockeeId === user.id
                ) as number) >= 0
              }
              friendship={chat?.activeRoomFriends.find(
                (friend) =>
                  friend.friendId === user.id || friend.userId === user.id
              )}
              user={user}
              key={user.id}
              isAdmin={chat.admins.findIndex((admin) => admin.userId === user.id) >= 0}
              isOwner={chat.activeRoom?.ownerId === user.id}
              isSelf={user.id === auth?.user?.id}
              isSelfAdmin={chat.admins.findIndex((admin) => admin.userId === auth?.user?.id) >= 0}
              isSelfOwner={chat?.activeRoom?.ownerId === auth?.user?.id}
              isGc={chat?.activeRoom?.type === "GC"}
            />
          ))}
        </div>
        <div className="my-3 py-3" id="bottom-members"></div>
      </div>

      {/* Time selection modal for mute / ban */}
      {
        chat?.openTimeModal && chat.userToAdminAction && chat.adminAction.length > 0 ?
          <TimeSelectionModal
            action={chat.adminAction}
            user={chat.userToAdminAction}
            setOpenModal={chat.setOpenTimeModal}
          /> :
          null
      }

      {/* Confirmation modal for owner transfer */}
      {
        chat?.openConfirmationModal && chat.userToAdminAction && chat.adminAction.length > 0 ?
          <ConfirmationModal
            action={chat.adminAction}
            user={chat.userToAdminAction}
            setOpenModal={chat.setOpenConfirmationModal}
          /> :
          null
      }
    </div>
  );
};

export default MemberList;
